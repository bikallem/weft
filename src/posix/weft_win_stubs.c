/* The stubs of weft_posix_stubs.c for Windows (design section 2.7, Windows):
   the clocks, the sockets, the pipes, random bytes, the errno values and the
   signal numbers. Each stub has the contract of its POSIX version: it never
   blocks, and it gives a value >= 0, or -errno with the errno values of the
   MinGW-w64 CRT. Each socket is non-blocking, and a child does not inherit
   it. A stub that gets WSAEWOULDBLOCK starts the poll of its direction
   (weft_win_io.c). */

#ifdef _WIN32

#include "weft_win.h"

#include <afunix.h>
#include <bcrypt.h>
#include <mstcpip.h>
#include <mswsock.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "weft_posix.h"

#ifndef SIO_UDP_CONNRESET
#define SIO_UDP_CONNRESET _WSAIOW(IOC_VENDOR, 12)
#endif

/* Errors */

int weft_win_errno(DWORD e) {
  switch (e) {
  case ERROR_FILE_NOT_FOUND:
  case ERROR_PATH_NOT_FOUND:
  case ERROR_INVALID_DRIVE:
  case ERROR_BAD_NETPATH:
  case ERROR_BAD_PATHNAME:
  case ERROR_INVALID_NAME:
  case ERROR_MOD_NOT_FOUND:
    return ENOENT;
  case ERROR_FILE_EXISTS:
  case ERROR_ALREADY_EXISTS:
    return EEXIST;
  case ERROR_DIRECTORY:
    return ENOTDIR;
  case ERROR_DIR_NOT_EMPTY:
    return ENOTEMPTY;
  case ERROR_ACCESS_DENIED:
  case ERROR_PRIVILEGE_NOT_HELD:
    return EACCES;
  case ERROR_INVALID_HANDLE:
    return EBADF;
  case ERROR_TOO_MANY_OPEN_FILES:
    return EMFILE;
  case ERROR_NOT_ENOUGH_MEMORY:
  case ERROR_OUTOFMEMORY:
  case ERROR_NO_SYSTEM_RESOURCES:
    return ENOMEM;
  case ERROR_BROKEN_PIPE:
  case ERROR_NO_DATA:
  case ERROR_PIPE_NOT_CONNECTED:
    return EPIPE;
  case ERROR_SHARING_VIOLATION:
  case ERROR_LOCK_VIOLATION:
  case ERROR_BUSY:
    return EBUSY;
  case ERROR_DISK_FULL:
  case ERROR_HANDLE_DISK_FULL:
    return ENOSPC;
  case ERROR_FILENAME_EXCED_RANGE:
    return ENAMETOOLONG;
  case ERROR_INVALID_PARAMETER:
  case ERROR_NEGATIVE_SEEK:
    return EINVAL;
  case ERROR_NOT_SAME_DEVICE:
    return EXDEV;
  case ERROR_BAD_EXE_FORMAT:
    return ENOEXEC;
  case ERROR_OPERATION_ABORTED:
    return ECANCELED;
  default:
    return WEFT_WIN32_BASE + (int)e;
  }
}

int weft_wsa_errno(int e) {
  switch (e) {
  case WSAEWOULDBLOCK:
    return EWOULDBLOCK;
  case WSAEINTR:
    return EINTR;
  case WSAEBADF:
  case WSA_INVALID_HANDLE:
    return EBADF;
  case WSAEACCES:
    return EACCES;
  case WSAEFAULT:
    return EFAULT;
  case WSAEINVAL:
    return EINVAL;
  case WSAEMFILE:
    return EMFILE;
  case WSAEINPROGRESS:
    return EINPROGRESS;
  case WSAEALREADY:
    return EALREADY;
  case WSAENOTSOCK:
    return ENOTSOCK;
  case WSAEDESTADDRREQ:
    return EDESTADDRREQ;
  case WSAEMSGSIZE:
    return EMSGSIZE;
  case WSAEPROTOTYPE:
    return EPROTOTYPE;
  case WSAENOPROTOOPT:
    return ENOPROTOOPT;
  case WSAEPROTONOSUPPORT:
    return EPROTONOSUPPORT;
  case WSAEOPNOTSUPP:
    return EOPNOTSUPP;
  case WSAEAFNOSUPPORT:
    return EAFNOSUPPORT;
  case WSAEADDRINUSE:
    return EADDRINUSE;
  case WSAEADDRNOTAVAIL:
    return EADDRNOTAVAIL;
  case WSAENETDOWN:
    return ENETDOWN;
  case WSAENETUNREACH:
    return ENETUNREACH;
  case WSAENETRESET:
    return ENETRESET;
  case WSAECONNABORTED:
    return ECONNABORTED;
  case WSAECONNRESET:
    return ECONNRESET;
  case WSAENOBUFS:
    return ENOBUFS;
  case WSAEISCONN:
    return EISCONN;
  case WSAENOTCONN:
    return ENOTCONN;
  case WSAESHUTDOWN:
    return EPIPE;
  case WSAETIMEDOUT:
    return ETIMEDOUT;
  case WSAECONNREFUSED:
    return ECONNREFUSED;
  case WSAENAMETOOLONG:
    return ENAMETOOLONG;
  case WSAEHOSTUNREACH:
    return EHOSTUNREACH;
  case WSA_NOT_ENOUGH_MEMORY:
    return ENOMEM;
  default:
    return WEFT_WIN32_BASE + e;
  }
}

static int wsa_err(void) { return -weft_wsa_errno(WSAGetLastError()); }

/* The time */

/* The monotonic clock: QueryPerformanceCounter (weft_posix.h). */
value weft_mono_ns(value unit) { return caml_copy_int64(weft_now_ns()); }

value weft_posix_ns(value unit) {
  FILETIME ft;
  GetSystemTimePreciseAsFileTime(&ft);
  return caml_copy_int64(weft_filetime_ns(ft));
}

/* Sockets */

/* [kind] is 0 for TCP and UDP, and 1 for a Unix socket. */
static int make_addr(int kind, value host, int port, struct sockaddr_storage *sa, int *len) {
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

static int inet_text(const struct sockaddr_storage *sa, char *buf, int *port) {
  if (sa->ss_family == AF_INET) {
    const struct sockaddr_in *in4 = (const struct sockaddr_in *)sa;
    inet_ntop(AF_INET, &in4->sin_addr, buf, INET6_ADDRSTRLEN);
    *port = ntohs(in4->sin_port);
    return 0;
  }
  if (sa->ss_family == AF_INET6) {
    const struct sockaddr_in6 *in6 = (const struct sockaddr_in6 *)sa;
    inet_ntop(AF_INET6, &in6->sin6_addr, buf, INET6_ADDRSTRLEN);
    *port = ntohs(in6->sin6_port);
    return 0;
  }
  return -1;
}

/* A non-blocking socket that a child does not inherit, in the table of the
   fds. A UDP socket does not get WSAECONNRESET from an ICMP message of an
   earlier sendto: on Linux, the datagram is lost with no error. */
static SOCKET new_socket(int family, int type, int *err) {
  weft_win_init();
  SOCKET s = WSASocketW(family, type, 0, NULL, 0, WSA_FLAG_OVERLAPPED | WSA_FLAG_NO_HANDLE_INHERIT);
  if (s == INVALID_SOCKET) {
    *err = wsa_err();
    return s;
  }
  u_long one = 1;
  ioctlsocket(s, FIONBIO, &one);
  if (type == SOCK_DGRAM) {
    BOOL no = FALSE;
    DWORD n;
    WSAIoctl(s, SIO_UDP_CONNRESET, &no, sizeof no, NULL, 0, &n, NULL, NULL);
  }
  int r = weft_win_add_socket(s);
  if (r < 0) {
    closesocket(s);
    *err = r;
    return INVALID_SOCKET;
  }
  return s;
}

/* The close of a new socket after an error: keep the error. */
static int drop(SOCKET s, int err) {
  weft_win_close((intptr_t)s);
  return err;
}

value weft_listen(value kind, value host, value port, value backlog) {
  struct sockaddr_storage sa;
  int len, err = 0;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_long(family);
  SOCKET s = new_socket(family, SOCK_STREAM, &err);
  if (s == INVALID_SOCKET) return Val_long(err);
  if (bind(s, (struct sockaddr *)&sa, len) != 0 || listen(s, Int_val(backlog)) != 0)
    return Val_long(drop(s, wsa_err()));
  return Val_long((intnat)s);
}

value weft_local_addr(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  int len = sizeof sa;
  char buf[INET6_ADDRSTRLEN] = "";
  const char *text = buf;
  size_t n = 0;
  int r = 0, port = 0;
  if (getsockname((SOCKET)Long_val(fd), (struct sockaddr *)&sa, &len) != 0) {
    r = wsa_err();
  } else if (inet_text(&sa, buf, &port) == 0) {
    n = strlen(buf);
  } else if (sa.ss_family == AF_UNIX) {
    size_t off = offsetof(struct sockaddr_un, sun_path);
    if (len > (int)sizeof sa) len = sizeof sa;
    text = ((struct sockaddr_un *)&sa)->sun_path;
    n = len > (int)off ? strnlen(text, len - off) : 0;
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

/* A connect that continues gives the socket, and the poll of its write edge
   starts at once: the end of the connect is a write edge. */
value weft_connect(value kind, value host, value port) {
  struct sockaddr_storage sa;
  int len, err = 0;
  int family = make_addr(Int_val(kind), host, Int_val(port), &sa, &len);
  if (family < 0) return Val_long(family);
  SOCKET s = new_socket(family, SOCK_STREAM, &err);
  if (s == INVALID_SOCKET) return Val_long(err);
  if (connect(s, (struct sockaddr *)&sa, len) != 0) {
    int e = WSAGetLastError();
    if (e != WSAEWOULDBLOCK && e != WSAEINPROGRESS) return Val_long(drop(s, -weft_wsa_errno(e)));
    weft_win_arm(s, WEFT_DIR_W);
  }
  return Val_long((intnat)s);
}

/* The result of a connect. Windows gives a peer to getpeername while the
   connect continues, so SO_CONNECT_TIME tells the end of the connect: it is
   0xFFFFFFFF until the socket connects. If the socket is not connected,
   SO_ERROR or the event of AFD gives the error, or the connect continues. A
   Unix socket has no SO_CONNECT_TIME: getpeername tells it. */
value weft_connect_result(value fd) {
  SOCKET s = (SOCKET)Long_val(fd);
  DWORD secs = 0;
  int sl = sizeof secs;
  if (getsockopt(s, SOL_SOCKET, SO_CONNECT_TIME, (char *)&secs, &sl) == 0) {
    if (secs != 0xFFFFFFFF) return Val_int(0);
  } else {
    struct sockaddr_storage sa;
    int len = sizeof sa;
    if (getpeername(s, (struct sockaddr *)&sa, &len) == 0) return Val_int(0);
    int pe = WSAGetLastError();
    if (pe != WSAENOTCONN) return Val_int(-weft_wsa_errno(pe));
  }
  int e = 0, l = sizeof e;
  if (getsockopt(s, SOL_SOCKET, SO_ERROR, (char *)&e, &l) != 0) return Val_int(wsa_err());
  if (e != 0) return Val_int(-weft_wsa_errno(e));
  int c = weft_win_connect_error(s);
  if (c != 0) return Val_int(-c);
  weft_win_arm(s, WEFT_DIR_W);
  return Val_int(-EAGAIN);
}

value weft_accept(value lfd) {
  CAMLparam1(lfd);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  int len = sizeof sa;
  char buf[INET6_ADDRSTRLEN] = "";
  int port = 0;
  intnat r;
  SOCKET l = (SOCKET)Long_val(lfd);
  SOCKET s = accept(l, (struct sockaddr *)&sa, &len);
  if (s == INVALID_SOCKET) {
    int e = WSAGetLastError();
    if (e == WSAEWOULDBLOCK) weft_win_arm(l, WEFT_DIR_R);
    r = -weft_wsa_errno(e);
  } else {
    /* The new socket gets the flags of the listener, but not O_NONBLOCK on
       each version: set it, and keep it from the children. */
    u_long one = 1;
    ioctlsocket(s, FIONBIO, &one);
    SetHandleInformation((HANDLE)s, HANDLE_FLAG_INHERIT, 0);
    int e = weft_win_add_socket(s);
    if (e < 0) {
      closesocket(s);
      r = e;
    } else {
      r = (intnat)s;
      inet_text(&sa, buf, &port);
    }
  }
  host = caml_copy_string(buf);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_long(r));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  Store_field(result, 3, Val_bool(r >= 0 && sa.ss_family == AF_UNIX));
  CAMLreturn(result);
}

/* UDP */

value weft_udp(value host, value port) {
  struct sockaddr_storage sa;
  int len, err = 0;
  int family = make_addr(0, host, Int_val(port), &sa, &len);
  if (family < 0) return Val_long(family);
  SOCKET s = new_socket(family, SOCK_DGRAM, &err);
  if (s == INVALID_SOCKET) return Val_long(err);
  if (bind(s, (struct sockaddr *)&sa, len) != 0) return Val_long(drop(s, wsa_err()));
  return Val_long((intnat)s);
}

static int clamp(intnat n) { return n > INT_MAX ? INT_MAX : (int)n; }

value weft_sendto(value fd, value buf, value off, value len, value dst) {
  struct sockaddr_storage sa;
  int alen;
  int family = make_addr(0, Field(dst, 0), Int_val(Field(dst, 1)), &sa, &alen);
  if (family < 0) return Val_long(family);
  SOCKET s = (SOCKET)Long_val(fd);
  int n = sendto(s, (const char *)Bytes_val(buf) + Long_val(off), clamp(Long_val(len)), 0,
                 (struct sockaddr *)&sa, alen);
  if (n == SOCKET_ERROR) {
    int e = WSAGetLastError();
    if (e == WSAEWOULDBLOCK) weft_win_arm(s, WEFT_DIR_W);
    return Val_long(-weft_wsa_errno(e));
  }
  return Val_long(n);
}

/* A datagram longer than the buffer gives WSAEMSGSIZE with the first bytes
   in the buffer: Linux gives these bytes with no error. */
value weft_recvfrom(value fd, value buf, value off, value len) {
  CAMLparam4(fd, buf, off, len);
  CAMLlocal2(result, host);
  struct sockaddr_storage sa;
  memset(&sa, 0, sizeof sa);
  int alen = sizeof sa;
  char text[INET6_ADDRSTRLEN] = "";
  int port = 0;
  SOCKET s = (SOCKET)Long_val(fd);
  intnat n;
  for (;;) {
    int k = clamp(Long_val(len));
    n = recvfrom(s, (char *)Bytes_val(buf) + Long_val(off), k, 0, (struct sockaddr *)&sa, &alen);
    if (n != SOCKET_ERROR) break;
    int e = WSAGetLastError();
    if (e == WSAEMSGSIZE) {
      n = k;
      break;
    }
    /* An ICMP message of an earlier sendto: the datagram is lost. */
    if (e == WSAECONNRESET) continue;
    if (e == WSAEWOULDBLOCK) weft_win_arm(s, WEFT_DIR_R);
    n = -weft_wsa_errno(e);
    break;
  }
  if (n >= 0) inet_text(&sa, text, &port);
  host = caml_copy_string(text);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_long(n));
  Store_field(result, 1, host);
  Store_field(result, 2, Val_int(port));
  CAMLreturn(result);
}

/* Socket options: 0 TCP_NODELAY, 1 SO_KEEPALIVE, 2 SO_BROADCAST, 3 SO_RCVBUF
   and 4 SO_SNDBUF. */
static void sockopt_of(int code, int *level, int *name) {
  static const int names[] = {TCP_NODELAY, SO_KEEPALIVE, SO_BROADCAST, SO_RCVBUF, SO_SNDBUF};
  *level = code == 0 ? IPPROTO_TCP : SOL_SOCKET;
  *name = names[code];
}

value weft_setsockopt(value fd, value code, value v) {
  int level, name, x = Int_val(v);
  sockopt_of(Int_val(code), &level, &name);
  return Val_int(setsockopt((SOCKET)Long_val(fd), level, name, (const char *)&x, sizeof x) != 0
                     ? wsa_err()
                     : 0);
}

value weft_getsockopt(value fd, value code) {
  int level, name, x = 0, l = sizeof x;
  sockopt_of(Int_val(code), &level, &name);
  if (getsockopt((SOCKET)Long_val(fd), level, name, (char *)&x, &l) != 0) return Val_int(wsa_err());
  /* A BOOL option can have one byte. */
  if (l == 1) x = *(unsigned char *)&x;
  return Val_int(x);
}

value weft_recv(value fd, value buf, value off, value len) {
  SOCKET s = (SOCKET)Long_val(fd);
  int n = recv(s, (char *)Bytes_val(buf) + Long_val(off), clamp(Long_val(len)), 0);
  if (n == SOCKET_ERROR) {
    int e = WSAGetLastError();
    if (e == WSAEWOULDBLOCK) weft_win_arm(s, WEFT_DIR_R);
    /* A local abort is a reset of the connection. */
    if (e == WSAECONNABORTED) e = WSAECONNRESET;
    if (e == WSAECONNRESET && weft_win_reset(s)) return Val_long(0);
    return Val_long(-weft_wsa_errno(e));
  }
  return Val_long(n);
}

/* A send on a UDP socket with no peer: Linux gives EDESTADDRREQ. */
value weft_send(value fd, value buf, value off, value len) {
  SOCKET s = (SOCKET)Long_val(fd);
  int n = send(s, (const char *)Bytes_val(buf) + Long_val(off), clamp(Long_val(len)), 0);
  if (n == SOCKET_ERROR) {
    int e = WSAGetLastError();
    if (e == WSAEWOULDBLOCK) weft_win_arm(s, WEFT_DIR_W);
    if (e == WSAECONNABORTED) e = WSAECONNRESET;
    if (e == WSAECONNRESET && weft_win_reset(s)) return Val_long(-EPIPE);
    if (e == WSAENOTCONN) {
      int type = 0, l = sizeof type;
      if (getsockopt(s, SOL_SOCKET, SO_TYPE, (char *)&type, &l) == 0 && type == SOCK_DGRAM)
        return Val_long(-EDESTADDRREQ);
    }
    return Val_long(-weft_wsa_errno(e));
  }
  return Val_long(n);
}

value weft_shutdown_send(value fd) {
  return Val_int(shutdown((SOCKET)Long_val(fd), SD_SEND) != 0 ? wsa_err() : 0);
}

value weft_reset_on_close(value fd) {
  struct linger l = {1, 0};
  return Val_int(setsockopt((SOCKET)Long_val(fd), SOL_SOCKET, SO_LINGER, (const char *)&l,
                            sizeof l) != 0
                     ? wsa_err()
                     : 0);
}

/* Pipes and fds (weft_win_io.c) */

value weft_pipe(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  intptr_t a = -1, b = -1;
  int e = weft_win_pipe(&a, &b);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_long(e < 0 ? e : a));
  Store_field(r, 1, Val_long(e < 0 ? -1 : b));
  CAMLreturn(r);
}

value weft_read(value fd, value buf, value off, value len) {
  return Val_long(weft_win_pipe_read(Long_val(fd), (char *)Bytes_val(buf) + Long_val(off),
                                     Long_val(len)));
}

value weft_write(value fd, value buf, value off, value len) {
  return Val_long(weft_win_pipe_write(Long_val(fd), (const char *)Bytes_val(buf) + Long_val(off),
                                      Long_val(len)));
}

value weft_close(value fd) { return Val_int(weft_win_close(Long_val(fd))); }

value weft_getrandom(value buf) {
  NTSTATUS st = BCryptGenRandom(NULL, Bytes_val(buf), (ULONG)caml_string_length(buf),
                                BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  return Val_int(st == 0 ? 0 : -EIO);
}

/* Errors */

value weft_errno_codes(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int codes[] = {EAGAIN, EBADF,  ECONNREFUSED, ECONNRESET, EPIPE,  EADDRINUSE,
                 EWOULDBLOCK, ENOENT, EEXIST, ENOTDIR, EISDIR, ENOTEMPTY,
                 EMFILE, ENFILE, ENOBUFS, ENOMEM};
  int n = sizeof codes / sizeof codes[0];
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_int(codes[i]));
  CAMLreturn(r);
}

/* A reset of a client before the accept gives WSAECONNRESET on Windows. */
value weft_accept_retry_codes(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int codes[] = {ECONNABORTED, ECONNRESET};
  int n = sizeof codes / sizeof codes[0];
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_int(codes[i]));
  CAMLreturn(r);
}

value weft_strerror(value code) {
  int c = Int_val(code);
  char buf[64];
  if (c >= WEFT_WIN32_BASE) {
    snprintf(buf, sizeof buf, "Win32 error %d", c - WEFT_WIN32_BASE);
    return caml_copy_string(buf);
  }
  return caml_copy_string(strerror(c));
}

#define E(x) {x, #x}
static const struct {
  int code;
  const char *name;
} names[] = {E(EPERM),        E(ENOENT),        E(ESRCH),        E(EINTR),
             E(EIO),          E(ENXIO),         E(E2BIG),        E(ENOEXEC),
             E(EBADF),        E(ECHILD),        E(EAGAIN),       E(ENOMEM),
             E(EACCES),       E(EFAULT),        E(EBUSY),        E(EEXIST),
             E(EXDEV),        E(ENODEV),        E(ENOTDIR),      E(EISDIR),
             E(EINVAL),       E(ENFILE),        E(EMFILE),       E(ENOTTY),
             E(EFBIG),        E(ENOSPC),        E(ESPIPE),       E(EROFS),
             E(EMLINK),       E(EPIPE),         E(EDOM),         E(ERANGE),
             E(EDEADLK),      E(ENAMETOOLONG),  E(ENOLCK),       E(ENOSYS),
             E(ENOTEMPTY),    E(ELOOP),         E(ENOTSOCK),     E(EDESTADDRREQ),
             E(EMSGSIZE),     E(EPROTOTYPE),    E(ENOPROTOOPT),  E(EPROTONOSUPPORT),
             E(EOPNOTSUPP),   E(EAFNOSUPPORT),  E(EADDRINUSE),   E(EADDRNOTAVAIL),
             E(ENETDOWN),     E(ENETUNREACH),   E(ENETRESET),    E(ECONNABORTED),
             E(ECONNRESET),   E(ENOBUFS),       E(EISCONN),      E(ENOTCONN),
             E(ETIMEDOUT),    E(ECONNREFUSED),  E(EHOSTUNREACH), E(EALREADY),
             E(EINPROGRESS),  E(ECANCELED),     E(EWOULDBLOCK),  E(EPROTO)};
#undef E

value weft_errname(value code) {
  int c = Int_val(code);
  char buf[32];
  for (size_t i = 0; i < sizeof names / sizeof names[0]; i++)
    if (names[i].code == c) return caml_copy_string(names[i].name);
  if (c >= WEFT_WIN32_BASE) {
    snprintf(buf, sizeof buf, "WIN32_%d", c - WEFT_WIN32_BASE);
    return caml_copy_string(buf);
  }
  return caml_copy_string(strerror(c));
}

/* Signals. The numbers of the CRT, and the numbers of Linux for the signals
   that the CRT does not have: they are only names on Windows. */
static const int numbers[] = {1 /* SIGHUP */,  SIGINT,          3 /* SIGQUIT */,
                              SIGILL,          SIGABRT,         SIGFPE,
                              9 /* SIGKILL */, SIGSEGV,         7 /* SIGBUS */,
                              13 /* SIGPIPE */, 14 /* SIGALRM */, SIGTERM,
                              10 /* SIGUSR1 */, 12 /* SIGUSR2 */, 17 /* SIGCHLD */,
                              18 /* SIGCONT */, 19 /* SIGSTOP */, 20 /* SIGTSTP */,
                              28 /* SIGWINCH */};

value weft_signal_numbers(value unit) {
  CAMLparam0();
  CAMLlocal1(r);
  int n = sizeof numbers / sizeof numbers[0];
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_int(numbers[i]));
  CAMLreturn(r);
}

/* The default action of an event of the console (SIGINT, SIGBREAK and
   SIGHUP, weft_win_io.c) ends the process as the console does. Another
   signal of the CRT gets its action with raise. The others have no action
   on Windows. */
value weft_resend(value sig) {
  int s = Int_val(sig);
  if (s == SIGINT || s == SIGBREAK || s == 1) ExitProcess(0xC000013A /* STATUS_CONTROL_C_EXIT */);
  if (s == SIGILL || s == SIGABRT || s == SIGFPE || s == SIGSEGV || s == SIGTERM)
    return Val_int(raise(s) != 0 ? -EINVAL : 0);
  return Val_int(-EINVAL);
}

#endif
