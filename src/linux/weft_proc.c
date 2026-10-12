/* Child processes and signals of the Linux world (design section 2.7,
   Signals). A child comes from pidfd_spawnp (weft_spawn.c of weft.posix), so
   its pidfd exists before any code can reap it. The pidfd is readable when
   the child exits, and waitid (P_PIDFD) ends the zombie. Signals come through
   a signalfd. */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
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

static void set_of(value sigs, sigset_t *set) {
  sigemptyset(set);
  for (mlsize_t i = 0; i < Wosize_val(sigs); i++) sigaddset(set, Int_val(Field(sigs, i)));
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
