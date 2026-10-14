/* The poller of the Linux world (design section 2.7): epoll, eventfd and
   timerfd. The stubs never block, except epoll_wait with no timeout, which
   releases the runtime lock. Each fd has the close-on-exec flag. A stub gives
   a value >= 0, or -errno. The other stubs are in src/posix. */

#define _GNU_SOURCE
#include <errno.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#define ERR (-errno)

/* The poller of a domain */

value weft_epoll_create(value unit) {
  int fd = epoll_create1(EPOLL_CLOEXEC);
  return Val_int(fd < 0 ? ERR : fd);
}

value weft_eventfd(value unit) {
  int fd = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
  return Val_int(fd < 0 ? ERR : fd);
}

/* A timerfd on CLOCK_MONOTONIC, or on CLOCK_REALTIME for the Posix alarm
   (M3). */
value weft_timerfd(value posix) {
  clockid_t c = Bool_val(posix) ? CLOCK_REALTIME : CLOCK_MONOTONIC;
  int fd = timerfd_create(c, TFD_NONBLOCK | TFD_CLOEXEC);
  return Val_int(fd < 0 ? ERR : fd);
}

/* Arm a timerfd at an absolute time. A time <= 0 disarms it. The Posix alarm
   has TFD_TIMER_CANCEL_ON_SET: a step of the clock makes the fd readable, and
   the read gives ECANCELED. */
value weft_timerfd_set(value fd, value posix, value ns) {
  int64_t t = Int64_val(ns);
  struct itimerspec its;
  memset(&its, 0, sizeof its);
  if (t > 0) {
    its.it_value.tv_sec = t / 1000000000;
    its.it_value.tv_nsec = t % 1000000000;
  }
  int flags = TFD_TIMER_ABSTIME | (Bool_val(posix) ? TFD_TIMER_CANCEL_ON_SET : 0);
  if (timerfd_settime(Int_val(fd), flags, &its, NULL) < 0) return Val_int(ERR);
  return Val_int(0);
}

/* Read the counter of an eventfd or a timerfd, to clear it. */
value weft_drain(value fd) {
  uint64_t n;
  ssize_t r = read(Int_val(fd), &n, sizeof n);
  return Val_int(r < 0 ? ERR : 0);
}

value weft_eventfd_signal(value fd) {
  uint64_t one = 1;
  ssize_t r = write(Int_val(fd), &one, sizeof one);
  return Val_int(r < 0 ? ERR : 0);
}

static int ctl(int epfd, int op, int fd, int64_t token, uint32_t events) {
  struct epoll_event ev;
  memset(&ev, 0, sizeof ev);
  ev.events = events;
  ev.data.u64 = (uint64_t)token;
  return epoll_ctl(epfd, op, fd, &ev) < 0 ? ERR : 0;
}

#define FD_EVENTS (EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET)

value weft_epoll_add(value epfd, value fd, value token) {
  return Val_int(
      ctl(Int_val(epfd), EPOLL_CTL_ADD, Int_val(fd), Long_val(token), FD_EVENTS));
}

/* The eventfd and the timerfd of a domain: EPOLLIN only. With EPOLLOUT, each
   drain of a count gives one more event. */
value weft_epoll_add_in(value epfd, value fd, value token) {
  return Val_int(ctl(Int_val(epfd), EPOLL_CTL_ADD, Int_val(fd), Long_val(token),
                     EPOLLIN | EPOLLET));
}

value weft_epoll_del(value epfd, value fd) {
  return Val_int(ctl(Int_val(epfd), EPOLL_CTL_DEL, Int_val(fd), 0, 0));
}

#define MAX_EVENTS 256

/* [weft_epoll_wait epfd sleep] gives an array of (token, read, write). A
   token is an OCaml int. With
   [sleep], it waits with no timeout and releases the runtime lock. */
value weft_epoll_wait(value epfd, value sleep) {
  CAMLparam2(epfd, sleep);
  CAMLlocal2(result, item);
  struct epoll_event evs[MAX_EVENTS];
  int fd = Int_val(epfd), n;
  if (Bool_val(sleep)) {
    caml_enter_blocking_section();
    n = epoll_wait(fd, evs, MAX_EVENTS, -1);
    caml_leave_blocking_section();
  } else {
    n = epoll_wait(fd, evs, MAX_EVENTS, 0);
  }
  if (n < 0) {
    /* EINTR: the loop polls again. Each other error is a bug of the world. */
    if (errno != EINTR) caml_failwith("epoll_wait");
    n = 0;
  }
  result = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) {
    uint32_t e = evs[i].events;
    item = caml_alloc_tuple(3);
    Store_field(item, 0, Val_long((intnat)evs[i].data.u64));
    Store_field(item, 1, Val_bool(e & (EPOLLIN | EPOLLRDHUP | EPOLLHUP | EPOLLERR)));
    Store_field(item, 2, Val_bool(e & (EPOLLOUT | EPOLLHUP | EPOLLERR)));
    Store_field(result, i, item);
  }
  CAMLreturn(result);
}
