/* The poller of the kqueue world: macOS and FreeBSD (design section 2.7).
   Each domain has one kqueue. An fd has a read filter and a write filter with
   EV_CLEAR, so its events are edges, as with EPOLLET. The alarm is the timeout
   of kevent. Signals come through EVFILT_SIGNAL, and the exit of a child
   through EVFILT_PROC. The stubs never block, except a wait with a timeout,
   which releases the runtime lock. A stub gives a value >= 0, or -errno. */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#define ERR (-errno)

extern char **environ;

value weft_kq_create(value unit) {
  int kq = kqueue();
  if (kq < 0) return Val_int(ERR);
  fcntl(kq, F_SETFD, FD_CLOEXEC);
  return Val_int(kq);
}

static int change(int kq, struct kevent *ch, int n) {
  return kevent(kq, ch, n, NULL, 0, NULL) < 0 ? ERR : 0;
}

/* The read and the write filters of an fd, with the token in udata. Each
   filter goes in its own call: an end of a pipe can refuse one of them. The
   result is an error only if the two filters fail. */
value weft_kq_add(value kq, value fd, value token) {
  struct kevent ch;
  void *t = (void *)(intptr_t)Long_val(token);
  EV_SET(&ch, Int_val(fd), EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, t);
  int r = change(Int_val(kq), &ch, 1);
  EV_SET(&ch, Int_val(fd), EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, t);
  int w = change(Int_val(kq), &ch, 1);
  return Val_int(r == 0 || w == 0 ? 0 : r);
}

/* The read filter only: the wake pipe of a domain. */
value weft_kq_add_in(value kq, value fd, value token) {
  struct kevent ch;
  EV_SET(&ch, Int_val(fd), EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0,
         (void *)(intptr_t)Long_val(token));
  return Val_int(change(Int_val(kq), &ch, 1));
}

/* A delete of a filter that does not exist gives ENOENT: the result is the
   same. */
value weft_kq_del(value kq, value fd) {
  struct kevent ch;
  int r = 0;
  EV_SET(&ch, Int_val(fd), EVFILT_READ, EV_DELETE, 0, 0, NULL);
  if (kevent(Int_val(kq), &ch, 1, NULL, 0, NULL) < 0 && errno != ENOENT) r = ERR;
  EV_SET(&ch, Int_val(fd), EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
  if (kevent(Int_val(kq), &ch, 1, NULL, 0, NULL) < 0 && errno != ENOENT) r = ERR;
  return Val_int(r);
}

/* EVFILT_SIGNAL records each attempt to give the signal, also if the signal
   is ignored (but not an ignored SIGCHLD). */
value weft_kq_signal(value kq, value signo, value token) {
  struct kevent ch;
  EV_SET(&ch, Int_val(signo), EVFILT_SIGNAL, EV_ADD | EV_CLEAR, 0, 0,
         (void *)(intptr_t)Long_val(token));
  return Val_int(change(Int_val(kq), &ch, 1));
}

/* The exit of a child. ESRCH if the child already exited. */
value weft_kq_proc(value kq, value pid, value token) {
  struct kevent ch;
  EV_SET(&ch, Int_val(pid), EVFILT_PROC, EV_ADD | EV_ONESHOT, NOTE_EXIT, 0,
         (void *)(intptr_t)Long_val(token));
  return Val_int(change(Int_val(kq), &ch, 1));
}

#define MAX_EVENTS 256

/* [weft_kq_wait kq ns] gives an array of (token, read, write, data). [ns] is
   the timeout: -1 for none, 0 for a poll. A wait that can block releases the
   runtime lock. [data] is the count of a signal. */
value weft_kq_wait(value kq, value ns) {
  CAMLparam2(kq, ns);
  CAMLlocal2(result, item);
  struct kevent evs[MAX_EVENTS];
  int64_t t = Int64_val(ns);
  struct timespec ts = {t / 1000000000, t % 1000000000}, *tp = t < 0 ? NULL : &ts;
  int n;
  if (t != 0) {
    caml_enter_blocking_section();
    n = kevent(Int_val(kq), NULL, 0, evs, MAX_EVENTS, tp);
    caml_leave_blocking_section();
  } else {
    n = kevent(Int_val(kq), NULL, 0, evs, MAX_EVENTS, tp);
  }
  if (n < 0) {
    /* EINTR: the loop polls again. Each other error is a bug of the world. */
    if (errno != EINTR) caml_failwith("kevent");
    n = 0;
  }
  result = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) {
    int err = evs[i].flags & EV_ERROR;
    int rd = evs[i].filter == EVFILT_READ || err;
    int wr = evs[i].filter == EVFILT_WRITE || err;
    item = caml_alloc_tuple(4);
    Store_field(item, 0, Val_long((intnat)(intptr_t)evs[i].udata));
    Store_field(item, 1, Val_bool(rd));
    Store_field(item, 2, Val_bool(wr));
    Store_field(item, 3, Val_long((intnat)evs[i].data));
    Store_field(result, i, item);
  }
  CAMLreturn(result);
}

/* Child processes */

/* A NULL-terminated copy of an array of strings. The strings stay in the OCaml
   heap: the call does not allocate in it, so the GC cannot move them. */
static char **c_strings(value a) {
  mlsize_t n = Wosize_val(a);
  char **c = malloc((n + 1) * sizeof *c);
  if (c == NULL) return NULL;
  for (mlsize_t i = 0; i < n; i++) c[i] = (char *)String_val(Field(a, i));
  c[n] = NULL;
  return c;
}

/* A pipe with FD_CLOEXEC on the two ends. */
static int cloexec_pipe(int p[2]) {
  if (pipe(p) < 0) return -1;
  fcntl(p[0], F_SETFD, FD_CLOEXEC);
  fcntl(p[1], F_SETFD, FD_CLOEXEC);
  return 0;
}

/* [args] is (prog, argv, env, has_env, cwd, stdio), as on Linux. The result is
   (pid, -1, stdin, stdout, stderr): the parent ends of the pipes, or -1. pid is
   -errno on an error. The world makes the fd of the exit. */
value weft_kq_spawn(value args) {
  CAMLparam1(args);
  CAMLlocal1(r);
  value prog = Field(args, 0), stdio = Field(args, 5);
  int ends[3] = {-1, -1, -1}, child[3] = {-1, -1, -1};
  pid_t pid = 0;
  int e = 0;
  posix_spawn_file_actions_t fa;
  posix_spawnattr_t at;
  sigset_t none, def;
  char **argv = c_strings(Field(args, 1));
  char **envp = Bool_val(Field(args, 3)) ? c_strings(Field(args, 2)) : environ;
  if (argv == NULL || envp == NULL) {
    free(argv);
    if (envp != environ) free(envp);
    caml_raise_out_of_memory();
  }
  posix_spawn_file_actions_init(&fa);
  posix_spawnattr_init(&at);
  for (int i = 0; i < 3 && e == 0; i++) {
    int kind = Int_val(Field(stdio, i));
    if (kind == 1) {
      e = posix_spawn_file_actions_addopen(&fa, i, "/dev/null", O_RDWR, 0);
    } else if (kind == 2) {
      int p[2];
      if (cloexec_pipe(p) < 0) {
        e = errno;
        break;
      }
      ends[i] = i == 0 ? p[1] : p[0];
      child[i] = i == 0 ? p[0] : p[1];
      fcntl(ends[i], F_SETFL, O_NONBLOCK);
      e = posix_spawn_file_actions_adddup2(&fa, child[i], i);
    }
  }
  if (e == 0 && caml_string_length(Field(args, 4)) > 0)
    e = posix_spawn_file_actions_addchdir_np(&fa, String_val(Field(args, 4)));
  if (e == 0) {
    /* The child gets no blocked signals, and the default action for the
       signals that the world ignores. */
    sigemptyset(&none);
    sigfillset(&def);
    sigdelset(&def, SIGKILL);
    sigdelset(&def, SIGSTOP);
    posix_spawnattr_setsigmask(&at, &none);
    posix_spawnattr_setsigdefault(&at, &def);
    posix_spawnattr_setflags(&at, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF);
    e = posix_spawnp(&pid, String_val(prog), &fa, &at, argv, envp);
  }
  posix_spawn_file_actions_destroy(&fa);
  posix_spawnattr_destroy(&at);
  free(argv);
  if (envp != environ) free(envp);
  for (int i = 0; i < 3; i++) {
    if (child[i] >= 0) close(child[i]);
    if (e != 0 && ends[i] >= 0) {
      close(ends[i]);
      ends[i] = -1;
    }
  }
  r = caml_alloc_tuple(5);
  Store_field(r, 0, Val_int(e != 0 ? -e : pid));
  Store_field(r, 1, Val_int(-1));
  for (int i = 0; i < 3; i++) Store_field(r, 2 + i, Val_int(ends[i]));
  CAMLreturn(r);
}

/* (kind, code): kind 0 runs, 1 exited with the code, 2 ended by the signal
   in code, or -errno. The call ends the zombie. */
value weft_kq_wait_child(value pid) {
  CAMLparam1(pid);
  CAMLlocal1(r);
  int st = 0, kind, code = 0;
  pid_t p = waitpid(Int_val(pid), &st, WNOHANG);
  if (p < 0) {
    kind = ERR;
  } else if (p == 0) {
    kind = 0;
  } else if (WIFEXITED(st)) {
    kind = 1;
    code = WEXITSTATUS(st);
  } else {
    kind = 2;
    code = WTERMSIG(st);
  }
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, Val_int(code));
  CAMLreturn(r);
}

value weft_kq_kill(value pid, value sig) {
  return Val_int(kill(Int_val(pid), Int_val(sig)) < 0 ? ERR : 0);
}
