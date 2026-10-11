/* The stubs of the Linux world (design section 2.7). They never block,
   except epoll_wait with no timeout, which releases the runtime lock. Each
   fd has the close-on-exec flag. A stub gives a value >= 0, or -errno. */

#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/random.h>
#include <sys/socket.h>
#include <sys/timerfd.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#define ERR (-errno)

/* The time */

static value clock_ns(clockid_t id) {
  struct timespec ts;
  clock_gettime(id, &ts);
  return caml_copy_int64((int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec);
}

value weft_mono_ns(value unit) { return clock_ns(CLOCK_MONOTONIC); }
value weft_posix_ns(value unit) { return clock_ns(CLOCK_REALTIME); }

/* The poller of a domain */

value weft_epoll_create(value unit) { return Val_int(epoll_create1(EPOLL_CLOEXEC)); }

value weft_eventfd(value unit) {
  return Val_int(eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC));
}

value weft_timerfd(value unit) {
  return Val_int(timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC));
}

/* Arm the timerfd at an absolute time on CLOCK_MONOTONIC. A time <= 0
   disarms it. */
value weft_timerfd_set(value fd, value ns) {
  int64_t t = Int64_val(ns);
  struct itimerspec its;
  memset(&its, 0, sizeof its);
  if (t > 0) {
    its.it_value.tv_sec = t / 1000000000;
    its.it_value.tv_nsec = t % 1000000000;
  }
  if (timerfd_settime(Int_val(fd), TFD_TIMER_ABSTIME, &its, NULL) < 0)
    return Val_int(ERR);
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

/* Sockets */

/* [kind] is 0 for TCP and 1 for a Unix socket. */
static int make_addr(int kind, value host, int port, struct sockaddr_storage *sa,
                     socklen_t *len) {
  memset(sa, 0, sizeof *sa);
  const char *h = String_val(host);
  if (kind == 1) {
    struct sockaddr_un *un = (struct sockaddr_un *)sa;
    if (caml_string_length(host) >= sizeof un->sun_path) return -ENAMETOOLONG;
    un->sun_family = AF_UNIX;
    strcpy(un->sun_path, h);
    *len = sizeof *un;
    return AF_UNIX;
  }
  if (port < 0 || port > 65535) return -EINVAL;
  if (strcmp(h, "localhost") == 0) h = "127.0.0.1";
  struct sockaddr_in *in4 = (struct sockaddr_in *)sa;
  if (inet_pton(AF_INET, h, &in4->sin_addr) == 1) {
    in4->sin_family = AF_INET;
    in4->sin_port = htons(port);
    *len = sizeof *in4;
    return AF_INET;
  }
  struct sockaddr_in6 *in6 = (struct sockaddr_in6 *)sa;
  if (inet_pton(AF_INET6, h, &in6->sin6_addr) == 1) {
    in6->sin6_family = AF_INET6;
    in6->sin6_port = htons(port);
    *len = sizeof *in6;
    return AF_INET6;
  }
  return -EINVAL;
}

static int new_socket(int family) {
  return socket(family, SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
}

value weft_listen(value kind, value host, value port, value backlog) {
  struct sockaddr_storage sa;
  socklen_t len;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_int(family);
  int fd = new_socket(family);
  if (fd < 0) return Val_int(ERR);
  int one = 1;
  if (family != AF_UNIX) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
  if (bind(fd, (struct sockaddr *)&sa, len) < 0 || listen(fd, Int_val(backlog)) < 0) {
    int e = ERR;
    close(fd);
    return Val_int(e);
  }
  return Val_int(fd);
}

/* The port of a socket that listens on TCP, for a listen on port 0. */
value weft_local_port(value fd) {
  struct sockaddr_storage sa;
  socklen_t len = sizeof sa;
  if (getsockname(Int_val(fd), (struct sockaddr *)&sa, &len) < 0) return Val_int(ERR);
  if (sa.ss_family == AF_INET) return Val_int(ntohs(((struct sockaddr_in *)&sa)->sin_port));
  if (sa.ss_family == AF_INET6)
    return Val_int(ntohs(((struct sockaddr_in6 *)&sa)->sin6_port));
  return Val_int(0);
}

value weft_connect(value kind, value host, value port) {
  struct sockaddr_storage sa;
  socklen_t len;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_int(family);
  int fd = new_socket(family);
  if (fd < 0) return Val_int(ERR);
  if (connect(fd, (struct sockaddr *)&sa, len) < 0 && errno != EINPROGRESS) {
    int e = ERR;
    close(fd);
    return Val_int(e);
  }
  return Val_int(fd);
}

/* The result of a connect. SO_ERROR is 0 also while the connect continues,
   so a connected socket is one with a peer. */
value weft_connect_result(value fd) {
  int e = 0;
  socklen_t l = sizeof e;
  if (getsockopt(Int_val(fd), SOL_SOCKET, SO_ERROR, &e, &l) < 0) return Val_int(ERR);
  if (e != 0) return Val_int(-e);
  struct sockaddr_storage sa;
  socklen_t len = sizeof sa;
  if (getpeername(Int_val(fd), (struct sockaddr *)&sa, &len) == 0) return Val_int(0);
  return Val_int(errno == ENOTCONN ? -EAGAIN : ERR);
}

/* [weft_accept fd] gives (fd or -errno, host, port, unix). [unix] tells that
   the peer is a Unix socket. */
value weft_accept(value lfd) {
  CAMLparam1(lfd);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  socklen_t len = sizeof sa;
  char buf[INET6_ADDRSTRLEN] = "";
  int port = 0;
  int fd = accept4(Int_val(lfd), (struct sockaddr *)&sa, &len, SOCK_NONBLOCK | SOCK_CLOEXEC);
  if (fd < 0) {
    fd = ERR;
  } else if (sa.ss_family == AF_INET) {
    inet_ntop(AF_INET, &((struct sockaddr_in *)&sa)->sin_addr, buf, sizeof buf);
    port = ntohs(((struct sockaddr_in *)&sa)->sin_port);
  } else if (sa.ss_family == AF_INET6) {
    inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&sa)->sin6_addr, buf, sizeof buf);
    port = ntohs(((struct sockaddr_in6 *)&sa)->sin6_port);
  }
  host = caml_copy_string(buf);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_int(fd));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  Store_field(result, 3, Val_bool(fd >= 0 && sa.ss_family == AF_UNIX));
  CAMLreturn(result);
}

/* recv and send with MSG_DONTWAIT: the flag applies to the call, so another
   process that clears O_NONBLOCK cannot make them block (section 2.3). The
   runtime lock stays held, so the GC cannot move the buffer. */
value weft_recv(value fd, value buf, value off, value len) {
  ssize_t n = recv(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len), MSG_DONTWAIT);
  return Val_long(n < 0 ? ERR : n);
}

value weft_send(value fd, value buf, value off, value len) {
  ssize_t n = send(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len),
                   MSG_DONTWAIT | MSG_NOSIGNAL);
  return Val_long(n < 0 ? ERR : n);
}

/* A pipe that the library made: both ends have O_NONBLOCK. (r, w), or
   (-errno, -1). */
value weft_pipe(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int fds[2];
  r = caml_alloc_tuple(2);
  if (pipe2(fds, O_NONBLOCK | O_CLOEXEC) < 0) {
    Store_field(r, 0, Val_int(ERR));
    Store_field(r, 1, Val_int(-1));
  } else {
    Store_field(r, 0, Val_int(fds[0]));
    Store_field(r, 1, Val_int(fds[1]));
  }
  CAMLreturn(r);
}

/* read and write on a non-blocking fd that the library made. A write to a
   pipe with no reader gives EPIPE: the world ignores SIGPIPE. */
value weft_read(value fd, value buf, value off, value len) {
  ssize_t n = read(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len));
  return Val_long(n < 0 ? ERR : n);
}

value weft_write(value fd, value buf, value off, value len) {
  ssize_t n = write(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len));
  return Val_long(n < 0 ? ERR : n);
}

value weft_shutdown_send(value fd) {
  return Val_int(shutdown(Int_val(fd), SHUT_WR) < 0 ? ERR : 0);
}

value weft_close(value fd) { return Val_int(close(Int_val(fd)) < 0 ? ERR : 0); }

value weft_getrandom(value buf) {
  size_t len = caml_string_length(buf), off = 0;
  while (off < len) {
    ssize_t n = getrandom(Bytes_val(buf) + off, len - off, 0);
    if (n < 0) {
      if (errno == EINTR) continue;
      return Val_int(ERR);
    }
    off += n;
  }
  return Val_int(0);
}

/* errno values that the world maps to Op.error. */
value weft_errno_codes(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int codes[] = {EAGAIN, EBADF, ECONNREFUSED, ECONNRESET, EPIPE, EADDRINUSE, EWOULDBLOCK};
  r = caml_alloc_tuple(7);
  for (int i = 0; i < 7; i++) Store_field(r, i, Val_int(codes[i]));
  CAMLreturn(r);
}

value weft_strerror(value code) { return caml_copy_string(strerror(Int_val(code))); }
