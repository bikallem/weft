/* Child processes and signals of the Linux world (design section 2.7,
   Signals). A child comes from pidfd_spawnp, so its pidfd exists before any
   code can reap it. The pidfd is readable when the child exits, and
   waitid (P_PIDFD) ends the zombie. Signals come through a signalfd. */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/pidfd.h>
#include <sys/signalfd.h>
#include <sys/wait.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define ERR (-errno)

extern char **environ;

/* The numbers of the signals, in the order of the constructors of
   Op.signal. */
static const int numbers[] = {SIGHUP,  SIGINT,  SIGQUIT, SIGILL,  SIGABRT,
                              SIGFPE,  SIGKILL, SIGSEGV, SIGBUS,  SIGPIPE,
                              SIGALRM, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD,
                              SIGCONT, SIGSTOP, SIGTSTP, SIGWINCH};

value weft_signal_numbers(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int n = sizeof numbers / sizeof numbers[0];
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_int(numbers[i]));
  CAMLreturn(r);
}

static void set_of(value sigs, sigset_t *set) {
  sigemptyset(set);
  for (mlsize_t i = 0; i < Wosize_val(sigs); i++) sigaddset(set, Int_val(Field(sigs, i)));
}

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

/* [args] is (prog, argv, env, has_env, cwd, stdio, sigdef): stdio has a kind
   for the fds 0, 1 and 2 (0 inherit, 1 /dev/null, 2 a pipe), and sigdef has
   the signals that get their default action. The result is (pid, pidfd,
   stdin, stdout, stderr): the parent ends of the pipes, or -1. pid is -errno
   on an error. */
value weft_spawn(value args) {
  CAMLparam1(args);
  CAMLlocal1(r);
  value prog = Field(args, 0), stdio = Field(args, 5);
  int ends[3] = {-1, -1, -1}, child[3] = {-1, -1, -1};
  int pid = 0, pidfd = -1, e = 0;
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
      if (pipe2(p, O_CLOEXEC) < 0) {
        e = errno;
        break;
      }
      /* The child gets fd 0 as the read end, and the fds 1 and 2 as write
         ends. Only the end of the parent is non-blocking. */
      ends[i] = i == 0 ? p[1] : p[0];
      child[i] = i == 0 ? p[0] : p[1];
      fcntl(ends[i], F_SETFL, O_NONBLOCK);
      e = posix_spawn_file_actions_adddup2(&fa, child[i], i);
    }
  }
  if (e == 0 && caml_string_length(Field(args, 4)) > 0)
    e = posix_spawn_file_actions_addchdir_np(&fa, String_val(Field(args, 4)));
  if (e == 0) {
    /* The child gets no blocked signals, and the default action for SIGPIPE,
       which the world ignores. */
    sigemptyset(&none);
    set_of(Field(args, 6), &def);
    posix_spawnattr_setsigmask(&at, &none);
    posix_spawnattr_setsigdefault(&at, &def);
    posix_spawnattr_setflags(&at, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF);
    e = pidfd_spawnp(&pidfd, String_val(prog), &fa, &at, argv, envp);
  }
  if (e == 0) {
    pid = pidfd_getpid(pidfd);
    if (pid < 0) pid = 0;
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
  Store_field(r, 1, Val_int(pidfd));
  for (int i = 0; i < 3; i++) Store_field(r, 2 + i, Val_int(ends[i]));
  CAMLreturn(r);
}

/* (kind, code): kind 0 runs, 1 exited with the code, 2 ended by the signal
   in code, or -errno. */
value weft_wait_child(value pidfd) {
  CAMLparam1(pidfd);
  CAMLlocal1(r);
  siginfo_t info;
  int kind, code = 0;
  memset(&info, 0, sizeof info);
  if (waitid(P_PIDFD, Int_val(pidfd), &info, WEXITED | WNOHANG) < 0) {
    kind = ERR;
  } else if (info.si_pid == 0) {
    kind = 0;
  } else {
    kind = info.si_code == CLD_EXITED ? 1 : 2;
    code = info.si_status;
  }
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, Val_int(code));
  CAMLreturn(r);
}

value weft_pidfd_kill(value pidfd, value sig) {
  return Val_int(pidfd_send_signal(Int_val(pidfd), Int_val(sig), NULL, 0) < 0 ? ERR : 0);
}

/* Block the signals in the thread that calls it, and give the old mask. */
value weft_sigmask_block(value sigs) {
  CAMLparam1(sigs);
  CAMLlocal1(old);
  sigset_t set;
  old = caml_alloc_string(sizeof(sigset_t));
  set_of(sigs, &set);
  pthread_sigmask(SIG_BLOCK, &set, (sigset_t *)Bytes_val(old));
  CAMLreturn(old);
}

value weft_sigmask_restore(value old) {
  sigset_t set;
  memcpy(&set, Bytes_val(old), sizeof set);
  pthread_sigmask(SIG_SETMASK, &set, NULL);
  return Val_unit;
}

value weft_signalfd(value sigs) {
  sigset_t set;
  set_of(sigs, &set);
  int fd = signalfd(-1, &set, SFD_NONBLOCK | SFD_CLOEXEC);
  return Val_int(fd < 0 ? ERR : fd);
}

/* The count of each signal number (0 to 64) that came, with a read until
   EAGAIN: the fd is edge-triggered. A read consumes the signals. */
value weft_signalfd_read(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(r);
  struct signalfd_siginfo info[16];
  int counts[65];
  memset(counts, 0, sizeof counts);
  for (;;) {
    ssize_t k = read(Int_val(fd), info, sizeof info);
    if (k <= 0) break;
    for (size_t i = 0; i < (size_t)k / sizeof info[0]; i++)
      if (info[i].ssi_signo < 65) counts[info[i].ssi_signo]++;
  }
  r = caml_alloc_tuple(65);
  for (int i = 0; i < 65; i++) Store_field(r, i, Val_int(counts[i]));
  CAMLreturn(r);
}
