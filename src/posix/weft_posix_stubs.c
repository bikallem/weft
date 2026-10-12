/* The stubs that the POSIX worlds share (design section 2.7): the clocks,
   sockets, pipes, reads and writes, random bytes, errno names and signal
   numbers. They never block. Each fd has the close-on-exec flag and
   O_NONBLOCK. A stub gives a value >= 0, or -errno. They build on Linux,
   macOS and FreeBSD. */

#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>
#ifndef __APPLE__
#include <sys/random.h>
#endif

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "weft_posix.h"

/* Linux has no SO_NOSIGPIPE, and macOS has no MSG_NOSIGNAL. The world also
   ignores SIGPIPE. */
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

/* The time */

static value clock_ns(clockid_t id) {
  struct timespec ts;
  clock_gettime(id, &ts);
  return caml_copy_int64((int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec);
}

value weft_mono_ns(value unit) { return clock_ns(CLOCK_MONOTONIC); }
value weft_posix_ns(value unit) { return clock_ns(CLOCK_REALTIME); }

/* The flags of a new fd on a system that cannot set them at the create. */
__attribute__((unused)) static int set_flags(int fd) {
  int fl = fcntl(fd, F_GETFL);
  if (fl < 0 || fcntl(fd, F_SETFL, fl | O_NONBLOCK) < 0) return -1;
  return fcntl(fd, F_SETFD, FD_CLOEXEC) < 0 ? -1 : 0;
}

/* A new fd that failed its flags: close it, and keep errno. */
__attribute__((unused)) static int drop_fd(int fd) {
  int e = errno;
  close(fd);
  errno = e;
  return -1;
}

static int sock_flags(int fd) {
#ifdef SO_NOSIGPIPE
  int one = 1;
  setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
  return fd;
}

/* Sockets */

/* [kind] is 0 for TCP and UDP, and 1 for a Unix socket. */
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
#ifdef HAVE_SA_LEN
    un->sun_len = sizeof *un;
#endif
    return AF_UNIX;
  }
  if (port < 0 || port > 65535) return -EINVAL;
  if (strcmp(h, "localhost") == 0) h = "127.0.0.1";
  struct sockaddr_in *in4 = (struct sockaddr_in *)sa;
  if (inet_pton(AF_INET, h, &in4->sin_addr) == 1) {
    in4->sin_family = AF_INET;
    in4->sin_port = htons(port);
    *len = sizeof *in4;
#ifdef HAVE_SA_LEN
    in4->sin_len = sizeof *in4;
#endif
    return AF_INET;
  }
  struct sockaddr_in6 *in6 = (struct sockaddr_in6 *)sa;
  if (inet_pton(AF_INET6, h, &in6->sin6_addr) == 1) {
    in6->sin6_family = AF_INET6;
    in6->sin6_port = htons(port);
    *len = sizeof *in6;
#ifdef HAVE_SA_LEN
    in6->sin6_len = sizeof *in6;
#endif
    return AF_INET6;
  }
  return -EINVAL;
}

static int new_socket(int family, int type) {
#ifdef SOCK_NONBLOCK
  int fd = socket(family, type | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
#else
  int fd = socket(family, type, 0);
  if (fd >= 0 && set_flags(fd) < 0) return drop_fd(fd);
#endif
  return fd < 0 ? fd : sock_flags(fd);
}

value weft_listen(value kind, value host, value port, value backlog) {
  struct sockaddr_storage sa;
  socklen_t len;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_int(family);
  int fd = new_socket(family, SOCK_STREAM);
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

/* [weft_local_addr fd] gives (0 or -errno, host, port, unix), as
   [weft_accept]. For a Unix socket, the host is the path, or "" if the socket
   has no path. */
value weft_local_addr(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  socklen_t len = sizeof sa;
  char buf[INET6_ADDRSTRLEN] = "";
  const char *text = buf;
  size_t n = 0;
  int r = 0, port = 0;
  if (getsockname(Int_val(fd), (struct sockaddr *)&sa, &len) < 0) {
    r = ERR;
  } else if (sa.ss_family == AF_INET) {
    inet_ntop(AF_INET, &((struct sockaddr_in *)&sa)->sin_addr, buf, sizeof buf);
    port = ntohs(((struct sockaddr_in *)&sa)->sin_port);
    n = strlen(buf);
  } else if (sa.ss_family == AF_INET6) {
    inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&sa)->sin6_addr, buf, sizeof buf);
    port = ntohs(((struct sockaddr_in6 *)&sa)->sin6_port);
    n = strlen(buf);
  } else if (sa.ss_family == AF_UNIX) {
    /* The path can have no NUL byte at the end. A length more than the
       buffer tells that the OS truncated the address. */
    size_t off = offsetof(struct sockaddr_un, sun_path);
    if (len > sizeof sa) len = sizeof sa;
    text = ((struct sockaddr_un *)&sa)->sun_path;
    n = len > off ? strnlen(text, len - off) : 0;
  } else {
    r = -EAFNOSUPPORT;
  }
  host = caml_alloc_initialized_string(n, text);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_int(r));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  Store_field(result, 3, Val_bool(r == 0 && sa.ss_family == AF_UNIX));
  CAMLreturn(result);
}

value weft_connect(value kind, value host, value port) {
  struct sockaddr_storage sa;
  socklen_t len;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_int(family);
  int fd = new_socket(family, SOCK_STREAM);
  if (fd < 0) return Val_int(ERR);
  if (connect(fd, (struct sockaddr *)&sa, len) < 0 && errno != EINPROGRESS) {
    int e = ERR;
    close(fd);
    return Val_int(e);
  }
  return Val_int(fd);
}

/* The result of a connect: a connected socket is one with a peer. If there
   is no peer, SO_ERROR gives the error of the connect, or 0 while the
   connect continues. The kernel sets SO_ERROR before it removes the peer, so
   a failure that comes during the call is never lost. A connected socket
   keeps its SO_ERROR for the next read or write. With no peer, getpeername
   gives ENOTCONN, or EINVAL on macOS after a reset. */
value weft_connect_result(value fd) {
  struct sockaddr_storage sa;
  socklen_t len = sizeof sa;
  if (getpeername(Int_val(fd), (struct sockaddr *)&sa, &len) == 0) return Val_int(0);
  int pe = errno;
  int e = 0;
  socklen_t l = sizeof e;
  if (getsockopt(Int_val(fd), SOL_SOCKET, SO_ERROR, &e, &l) < 0) return Val_int(ERR);
  if (e != 0) return Val_int(-e);
  return Val_int(pe == ENOTCONN ? -EAGAIN : -pe);
}

/* [weft_accept fd] gives (fd or -errno, host, port, unix). [unix] tells that
   the peer is a Unix socket. */
value weft_accept(value lfd) {
  CAMLparam1(lfd);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  socklen_t len = sizeof sa;
  char buf[INET6_ADDRSTRLEN] = "";
  int port = 0;
#ifdef SOCK_NONBLOCK
  int fd = accept4(Int_val(lfd), (struct sockaddr *)&sa, &len, SOCK_NONBLOCK | SOCK_CLOEXEC);
#else
  int fd = accept(Int_val(lfd), (struct sockaddr *)&sa, &len);
  if (fd >= 0 && set_flags(fd) < 0) fd = drop_fd(fd);
#endif
  if (fd < 0) {
    fd = ERR;
  } else {
    sock_flags(fd);
    if (sa.ss_family == AF_INET) {
      inet_ntop(AF_INET, &((struct sockaddr_in *)&sa)->sin_addr, buf, sizeof buf);
      port = ntohs(((struct sockaddr_in *)&sa)->sin_port);
    } else if (sa.ss_family == AF_INET6) {
      inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&sa)->sin6_addr, buf, sizeof buf);
      port = ntohs(((struct sockaddr_in6 *)&sa)->sin6_port);
    }
  }
  host = caml_copy_string(buf);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_int(fd));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  Store_field(result, 3, Val_bool(fd >= 0 && sa.ss_family == AF_UNIX));
  CAMLreturn(result);
}

/* UDP */

/* A UDP socket, bound to the host and the port. */
value weft_udp(value host, value port) {
  struct sockaddr_storage sa;
  socklen_t len;
  int family = make_addr(0, host, Int_val(port), &sa, &len);
  if (family < 0) return Val_int(family);
  int fd = new_socket(family, SOCK_DGRAM);
  if (fd < 0) return Val_int(ERR);
  if (bind(fd, (struct sockaddr *)&sa, len) < 0) {
    int e = ERR;
    close(fd);
    return Val_int(e);
  }
  return Val_int(fd);
}

/* [dst] is (host, port). The runtime lock stays held, so the GC cannot move
   the buffer. */
value weft_sendto(value fd, value buf, value off, value len, value dst) {
  struct sockaddr_storage sa;
  socklen_t alen;
  int family = make_addr(0, Field(dst, 0), Int_val(Field(dst, 1)), &sa, &alen);
  if (family < 0) return Val_long(family);
  ssize_t n = sendto(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len),
                     MSG_DONTWAIT | MSG_NOSIGNAL, (struct sockaddr *)&sa, alen);
  return Val_long(n < 0 ? ERR : n);
}

/* (n or -errno, host, port). The recv comes before each allocation, so the
   buffer does not move. A socket with no address of the sender gives
   ("", 0). */
value weft_recvfrom(value fd, value buf, value off, value len) {
  CAMLparam4(fd, buf, off, len);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  socklen_t alen = sizeof sa;
  char text[INET6_ADDRSTRLEN] = "";
  int port = 0;
  ssize_t n = recvfrom(Int_val(fd), Bytes_val(buf) + Long_val(off), Long_val(len),
                       MSG_DONTWAIT, (struct sockaddr *)&sa, &alen);
  if (n < 0) {
    n = ERR;
  } else if (sa.ss_family == AF_INET) {
    inet_ntop(AF_INET, &((struct sockaddr_in *)&sa)->sin_addr, text, sizeof text);
    port = ntohs(((struct sockaddr_in *)&sa)->sin_port);
  } else if (sa.ss_family == AF_INET6) {
    inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&sa)->sin6_addr, text, sizeof text);
    port = ntohs(((struct sockaddr_in6 *)&sa)->sin6_port);
  }
  host = caml_copy_string(text);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_long(n));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  CAMLreturn(result);
}

/* Socket options. The codes are the order of [Op.sockopt]: 0 TCP_NODELAY,
   1 SO_KEEPALIVE, 2 SO_BROADCAST, 3 SO_RCVBUF and 4 SO_SNDBUF. */
static void sockopt_of(int code, int *level, int *name) {
  static const int names[] = {TCP_NODELAY, SO_KEEPALIVE, SO_BROADCAST, SO_RCVBUF, SO_SNDBUF};
  *level = code == 0 ? IPPROTO_TCP : SOL_SOCKET;
  *name = names[code];
}

value weft_setsockopt(value fd, value code, value v) {
  int level, name, x = Int_val(v);
  sockopt_of(Int_val(code), &level, &name);
  return Val_int(setsockopt(Int_val(fd), level, name, &x, sizeof x) < 0 ? ERR : 0);
}

/* The value, or -errno. Each value is 0 or more. */
value weft_getsockopt(value fd, value code) {
  int level, name, x = 0;
  socklen_t l = sizeof x;
  sockopt_of(Int_val(code), &level, &name);
  return Val_int(getsockopt(Int_val(fd), level, name, &x, &l) < 0 ? ERR : x);
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

value weft_shutdown_send(value fd) {
  return Val_int(shutdown(Int_val(fd), SHUT_WR) < 0 ? ERR : 0);
}

/* SO_LINGER on with a time of 0: close(2) then sends a reset (RST) and does
   not block. */
value weft_reset_on_close(value fd) {
  struct linger l = {1, 0};
  return Val_int(setsockopt(Int_val(fd), SOL_SOCKET, SO_LINGER, &l, sizeof l) < 0 ? ERR : 0);
}

/* Pipes and fds */

/* A pipe with O_NONBLOCK on the two ends. (r, w), or (-errno, -1). */
static int new_pipe(int fds[2]) {
#ifdef __APPLE__
  if (pipe(fds) < 0) return -1;
  if (set_flags(fds[0]) < 0 || set_flags(fds[1]) < 0) {
    drop_fd(fds[1]);
    return drop_fd(fds[0]);
  }
  return 0;
#else
  return pipe2(fds, O_NONBLOCK | O_CLOEXEC);
#endif
}

value weft_pipe(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int fds[2];
  r = caml_alloc_tuple(2);
  if (new_pipe(fds) < 0) {
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

value weft_close(value fd) { return Val_int(close(Int_val(fd)) < 0 ? ERR : 0); }

value weft_getrandom(value buf) {
  size_t len = caml_string_length(buf);
#ifdef __APPLE__
  arc4random_buf(Bytes_val(buf), len);
#else
  size_t off = 0;
  while (off < len) {
    ssize_t n = getrandom(Bytes_val(buf) + off, len - off, 0);
    if (n < 0) {
      if (errno == EINTR) continue;
      return Val_int(ERR);
    }
    off += n;
  }
#endif
  return Val_int(0);
}

/* Errors */

/* errno values that the world maps to Op.error. */
value weft_errno_codes(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int codes[] = {EAGAIN, EBADF,  ECONNREFUSED, ECONNRESET, EPIPE,  EADDRINUSE,
                 EWOULDBLOCK, ENOENT, EEXIST, ENOTDIR, EISDIR, ENOTEMPTY};
  int n = sizeof codes / sizeof codes[0];
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_int(codes[i]));
  CAMLreturn(r);
}

value weft_strerror(value code) { return caml_copy_string(strerror(Int_val(code))); }

/* The names of errno values, as the simulator gives them. The first name of a
   value wins: EAGAIN before EWOULDBLOCK, EOPNOTSUPP before ENOTSUP. */
#define E(x) {x, #x}
static const struct {
  int code;
  const char *name;
} names[] = {E(EPERM),        E(ENOENT),          E(ESRCH),         E(EINTR),
             E(EIO),          E(ENXIO),           E(E2BIG),         E(ENOEXEC),
             E(EBADF),        E(ECHILD),          E(EAGAIN),        E(ENOMEM),
             E(EACCES),       E(EFAULT),          E(EBUSY),         E(EEXIST),
             E(EXDEV),        E(ENODEV),          E(ENOTDIR),       E(EISDIR),
             E(EINVAL),       E(ENFILE),          E(EMFILE),        E(ENOTTY),
             E(ETXTBSY),      E(EFBIG),           E(ENOSPC),        E(ESPIPE),
             E(EROFS),        E(EMLINK),          E(EPIPE),         E(EDOM),
             E(ERANGE),       E(EDEADLK),         E(ENAMETOOLONG),  E(ENOLCK),
             E(ENOSYS),       E(ENOTEMPTY),       E(ELOOP),         E(ENOTSOCK),
             E(EDESTADDRREQ), E(EMSGSIZE),        E(EPROTOTYPE),    E(ENOPROTOOPT),
             E(EPROTONOSUPPORT), E(EOPNOTSUPP),   E(ENOTSUP),       E(EAFNOSUPPORT),
             E(EADDRINUSE),   E(EADDRNOTAVAIL),   E(ENETDOWN),      E(ENETUNREACH),
             E(ECONNABORTED), E(ECONNRESET),      E(ENOBUFS),       E(EISCONN),
             E(ENOTCONN),     E(ETIMEDOUT),       E(ECONNREFUSED),  E(EHOSTUNREACH),
             E(EALREADY),     E(EINPROGRESS),     E(ECANCELED),     E(EWOULDBLOCK)};
#undef E

value weft_errname(value code) {
  int c = Int_val(code);
  for (size_t i = 0; i < sizeof names / sizeof names[0]; i++)
    if (names[i].code == c) return caml_copy_string(names[i].name);
  return caml_copy_string(strerror(c));
}

/* Signals */

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
