/* The poller of the kqueue world: macOS and FreeBSD (design section 2.7).
   Each domain has one kqueue. An fd has a read filter and a write filter with
   EV_CLEAR, so its events are edges, as with EPOLLET. The alarm is the timeout
   of kevent. A handler of the world counts the signals, and the exit of a
   child comes through EVFILT_PROC. The stubs never block, except a wait with a timeout
   and the wait for a zombie, which release the runtime lock. A stub gives a
   value >= 0, or -errno. */

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <signal.h>
#include <stdatomic.h>
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

/* Signals (design section 2.7, Signals). A watched signal has the handler
   [on_signal]: it adds 1 to the count of the signal, then it writes the wake
   pipe of domain 0. Domain 0 reads the pipe, then takes the counts, so it
   sees each count before its write.

   The handler runs on any thread that does not block the signal, also during
   a wait of a domain: kevent then gives EINTR. SA_RESTART restarts most other
   calls, for example read, write and waitid. A call such as select or
   nanosleep in fiber code on a domain can give EINTR.

   The handler must run on the signal stack of the thread (SA_ONSTACK). OCaml
   code runs on small stacks of fibers, and a signal frame on such a stack
   corrupts the stack and the heap. OCaml gives a signal stack to each thread
   that it makes: the main thread, the thread of each domain and each
   systhread. The pools and the watchdog block all signals.

   The counts and the pipe are for the process, so one world at a time can
   watch signals: a world claims [sig_wake] with a CAS from -1. [sig_running]
   counts the handlers that run, so that the release waits until no handler
   can write the pipe. */

#define MAX_SIG 129

static atomic_int sig_counts[MAX_SIG];
static atomic_int sig_wake = -1;
static atomic_int sig_running;

static void on_signal(int s) {
  int e = errno;
  /* Before the load of [sig_wake]: a release that sees 0 comes before the
     load, which then gives -1. */
  atomic_fetch_add(&sig_running, 1);
  if (s > 0 && s < MAX_SIG) atomic_fetch_add(&sig_counts[s], 1);
  int fd = atomic_load(&sig_wake);
  if (fd >= 0) {
    char one = 1;
    ssize_t r = write(fd, &one, 1);
    (void)r;
  }
  atomic_fetch_sub(&sig_running, 1);
  errno = e;
}

/* Claim the handler for the wake pipe [fd] of domain 0. [false] if another
   world holds it. */
value weft_kq_sig_claim(value fd) {
  int none = -1;
  return Val_bool(atomic_compare_exchange_strong(&sig_wake, &none, Int_val(fd)));
}

/* Release the handler. After the return, no handler writes the pipe, so the
   world can close it. A handler is short and never blocks, so the wait is
   short. */
value weft_kq_sig_release(value unit) {
  atomic_store(&sig_wake, -1);
  while (atomic_load(&sig_running) > 0) sched_yield();
  return Val_unit;
}

/* Give the signal the handler, and give the old action. */
value weft_kq_sig_catch(value signo) {
  CAMLparam1(signo);
  CAMLlocal1(old);
  int s = Int_val(signo);
  struct sigaction sa, prev;
  if (s <= 0 || s >= MAX_SIG) caml_failwith("sigaction: EINVAL");
  old = caml_alloc_string(sizeof prev);
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = on_signal;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART | SA_ONSTACK;
  atomic_store(&sig_counts[s], 0);
  if (sigaction(s, &sa, &prev) < 0) caml_failwith("sigaction");
  memcpy(Bytes_val(old), &prev, sizeof prev);
  CAMLreturn(old);
}

/* Give the signal its old action again. After the return, the handler of the
   world gets no more of this signal, but a handler that started before can
   still add to the count. */
value weft_kq_sig_restore(value signo, value old) {
  struct sigaction prev;
  memcpy(&prev, Bytes_val(old), sizeof prev);
  sigaction(Int_val(signo), &prev, NULL);
  return Val_unit;
}

/* Take the count of a signal, and set it to 0. */
value weft_kq_sig_take(value signo) {
  return Val_int(atomic_exchange(&sig_counts[Int_val(signo)], 0));
}

/* The exit of a child. ESRCH if the child already exited. */
value weft_kq_proc(value kq, value pid, value token) {
  struct kevent ch;
  EV_SET(&ch, Int_val(pid), EVFILT_PROC, EV_ADD | EV_ONESHOT, NOTE_EXIT, 0,
         (void *)(intptr_t)Long_val(token));
  return Val_int(change(Int_val(kq), &ch, 1));
}

#define MAX_EVENTS 256

/* [weft_kq_wait kq ns] gives an array of (token, read, write). [ns] is the
   timeout: -1 for none, 0 for a poll. A wait that can block releases the
   runtime lock. */
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
    item = caml_alloc_tuple(3);
    Store_field(item, 0, Val_long((intnat)(intptr_t)evs[i].udata));
    Store_field(item, 1, Val_bool(rd));
    Store_field(item, 2, Val_bool(wr));
    Store_field(result, i, item);
  }
  CAMLreturn(result);
}

/* Read all bytes of a non-blocking fd, a wake pipe. */
value weft_drain_all(value fd) {
  char buf[256];
  while (read(Int_val(fd), buf, sizeof buf) > 0) {
  }
  return Val_unit;
}

/* Write one byte to a wake pipe or to the exit pipe of a child. A full pipe already wakes its reader. */
value weft_poke(value fd) {
  char one = 1;
  ssize_t r = write(Int_val(fd), &one, 1);
  (void)r;
  return Val_unit;
}

/* Child processes */

/* Wait until the child is a zombie, with no reap. The world calls it only
   after the kernel told of the exit, so the wait is short. EVFILT_PROC can
   report the exit before the child is a zombie, and then a reap with WNOHANG
   finds nothing. An error (ECHILD) ends the wait: the reap then gives it. */
value weft_kq_zombie(value pid) {
  siginfo_t info;
  int p = Int_val(pid), r;
  caml_enter_blocking_section();
  do {
    memset(&info, 0, sizeof info);
    r = waitid(P_PID, p, &info, WEXITED | WNOWAIT);
  } while (r < 0 && errno == EINTR);
  caml_leave_blocking_section();
  return Val_unit;
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
