/* The I/O of the Windows world (design section 2.7, Windows): the table of
   the fds, the edges of the sockets with AFD_POLL, the pipes, the children,
   the ports of the domains and their wait, and Ctrl-C.

   Each domain has a port: an I/O completion port (IOCP) and a
   high-resolution timer for its alarm. The world has one more port for its
   I/O, and a C thread that waits on it. That thread takes the completions of
   the polls, the pipes and the children, and it sends each edge to the
   domains that registered the fd, with a completion on their ports. Thus an
   edge reaches a domain only through its wait, as an event of epoll does,
   and no domain waits for the I/O of another domain.

   The sockets (the spike in tools/windows-vm/spike/afd_poll.c): AFD gives the
   events of a socket only to its oldest poll. Thus the table keeps at most
   one poll for each socket. A stub that gets WSAEWOULDBLOCK starts the poll
   for its direction (weft_win_arm), and the completion clears the direction.
   A poll that includes SEND completes at once on a writable socket, so a
   poll asks only for the directions that got WSAEWOULDBLOCK, as mio does.

   A Register gives one edge at once, for read and write: on Linux, an
   EPOLL_CTL_ADD of a ready fd gives an event at once. The first operation of
   a fiber can get WSAEWOULDBLOCK before the Register, and its poll can
   complete before the Register. The edge of the Register makes sure that the
   fiber tries again.

   The pipes: Windows has no non-blocking pipe. A pipe is a named pipe with
   overlapped I/O, and each end of the world has a buffer. A read starts one
   overlapped read into the buffer and gives EAGAIN; its completion gives the
   read edge. A write copies the data into the buffer, starts one overlapped
   write and gives the count; its completion gives the write edge. */

#ifdef _WIN32

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winternl.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include "weft_win.h"

/* AFD_POLL (wepoll and mio use these definitions). */

#define IOCTL_AFD_POLL 0x00012024
#define AFD_POLL_RECEIVE 0x0001
#define AFD_POLL_RECEIVE_EXPEDITED 0x0002
#define AFD_POLL_SEND 0x0004
#define AFD_POLL_DISCONNECT 0x0008
#define AFD_POLL_ABORT 0x0010
#define AFD_POLL_LOCAL_CLOSE 0x0020
#define AFD_POLL_ACCEPT 0x0080
#define AFD_POLL_CONNECT_FAIL 0x0100

#define AFD_READ_EVENTS                                                      \
  (AFD_POLL_RECEIVE | AFD_POLL_RECEIVE_EXPEDITED | AFD_POLL_DISCONNECT |     \
   AFD_POLL_ABORT | AFD_POLL_LOCAL_CLOSE | AFD_POLL_ACCEPT | AFD_POLL_CONNECT_FAIL)
#define AFD_WRITE_EVENTS                                                   \
  (AFD_POLL_SEND | AFD_POLL_DISCONNECT | AFD_POLL_ABORT | AFD_POLL_LOCAL_CLOSE | \
   AFD_POLL_CONNECT_FAIL)

#ifndef SIO_BASE_HANDLE
#define SIO_BASE_HANDLE _WSAIOR(IOC_WS2, 34)
#endif

#define W_STATUS_PENDING ((NTSTATUS)0x00000103L)
#define W_STATUS_CANCELLED ((NTSTATUS)0xC0000120L)
#define W_STATUS_PIPE_BROKEN ((NTSTATUS)0xC000014BL)
#define W_STATUS_CONNECTION_REFUSED ((NTSTATUS)0xC0000236L)

typedef struct {
  HANDLE Handle;
  ULONG Events;
  NTSTATUS Status;
} AFD_POLL_HANDLE_INFO;

typedef struct {
  LARGE_INTEGER Timeout;
  ULONG NumberOfHandles;
  ULONG Exclusive;
  AFD_POLL_HANDLE_INFO Handles[1];
} AFD_POLL_INFO;

typedef NTSTATUS(NTAPI *nt_create_file_t)(PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES,
                                          PIO_STATUS_BLOCK, PLARGE_INTEGER, ULONG, ULONG,
                                          ULONG, ULONG, PVOID, ULONG);
typedef NTSTATUS(NTAPI *nt_ioctl_t)(HANDLE, HANDLE, PVOID, PVOID, PIO_STATUS_BLOCK, ULONG,
                                    PVOID, ULONG, PVOID, ULONG);
typedef NTSTATUS(NTAPI *nt_create_packet_t)(PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES);
typedef NTSTATUS(NTAPI *nt_associate_t)(HANDLE, HANDLE, HANDLE, PVOID, PVOID, NTSTATUS,
                                        ULONG_PTR, PBOOLEAN);
typedef NTSTATUS(NTAPI *nt_cancel_packet_t)(HANDLE, BOOLEAN);
typedef ULONG(NTAPI *rtl_status_t)(NTSTATUS);

static nt_create_file_t nt_create_file;
static nt_ioctl_t nt_ioctl;
static nt_create_packet_t nt_create_packet;
static nt_associate_t nt_associate;
static nt_cancel_packet_t nt_cancel_packet;
static rtl_status_t rtl_status;

static INIT_ONCE init_once = INIT_ONCE_STATIC_INIT;

static BOOL CALLBACK init_fn(PINIT_ONCE o, PVOID p, PVOID *c) {
  WSADATA wsa;
  WSAStartup(MAKEWORD(2, 2), &wsa);
  HMODULE nt = GetModuleHandleW(L"ntdll.dll");
  nt_create_file = (nt_create_file_t)(void *)GetProcAddress(nt, "NtCreateFile");
  nt_ioctl = (nt_ioctl_t)(void *)GetProcAddress(nt, "NtDeviceIoControlFile");
  nt_create_packet =
      (nt_create_packet_t)(void *)GetProcAddress(nt, "NtCreateWaitCompletionPacket");
  nt_associate = (nt_associate_t)(void *)GetProcAddress(nt, "NtAssociateWaitCompletionPacket");
  nt_cancel_packet =
      (nt_cancel_packet_t)(void *)GetProcAddress(nt, "NtCancelWaitCompletionPacket");
  rtl_status = (rtl_status_t)(void *)GetProcAddress(nt, "RtlNtStatusToDosError");
  return TRUE;
}

void weft_win_init(void) { InitOnceExecuteOnce(&init_once, init_fn, NULL, NULL); }

static int status_errno(NTSTATUS st) {
  if (st == W_STATUS_CONNECTION_REFUSED) return ECONNREFUSED;
  return weft_win_errno(rtl_status ? rtl_status(st) : ERROR_GEN_FAILURE);
}

wchar_t *weft_wide(const char *s) {
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s, -1, NULL, 0);
  if (n <= 0) return NULL;
  wchar_t *w = malloc(n * sizeof *w);
  if (w == NULL) return NULL;
  MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s, -1, w, n);
  return w;
}


#define MAX_EVENTS 128

/* The keys of the completions. A domain gets WEFT_KEY_WAKE (weft_win.h),
   KEY_EDGE and KEY_TIMER on its port. The I/O thread of the world gets the
   others on the port of the world. */
#define KEY_EDGE 2
#define KEY_AFD 3
#define KEY_PIPE 4
#define KEY_PROC 5
#define KEY_TIMER 6
#define KEY_STOP 7

/* The worlds and their ports */

struct port {
  HANDLE iocp;
  HANDLE timer;        /* The alarm: a high-resolution waitable timer. */
  HANDLE timer_packet; /* Connects the timer to the IOCP. */
  int timer_assoc;     /* The packet waits on the timer. Only the domain uses it. */
};

struct world {
  long id; /* A new number for each world: an address can come again. */
  struct world *next;
  int closing;
  int outstanding; /* The I/O of the world that did not complete. */
  HANDLE io_iocp;  /* The port of the I/O of the world, and its thread. */
  HANDLE io_thread;
  HANDLE afd; /* The AFD handle of the polls, on io_iocp. */
  int n;
  struct port ports[];
};

/* One lock (an SRW lock) covers the list of the worlds, the table of the fds
   and each entry. The stubs take it only on a slow path: an EAGAIN, a close,
   a Register, a pipe. */
static SRWLOCK lock = SRWLOCK_INIT;

static void lock_x(void) { AcquireSRWLockExclusive(&lock); }
static void unlock_x(void) { ReleaseSRWLockExclusive(&lock); }

/* The open worlds, the newest first. */
static struct world *worlds = NULL;
static long world_ids = 0;

/* The world of the calling thread: the last world that the thread made or
   waited in. A domain waits in its world only, so its stubs use its world. */
static __thread struct world *tls_world;
static __thread long tls_id;

/* Under the lock. */
static int alive(struct world *w, long id) {
  for (struct world *x = worlds; x != NULL; x = x->next)
    if (x == w && x->id == id) return 1;
  return 0;
}

/* The world of the new I/O of the calling thread: its own world, or the
   newest open world. Under the lock. */
static struct world *cur_locked(void) {
  if (tls_world != NULL && alive(tls_world, tls_id)) return tls_world;
  return worlds;
}

static struct world *cur_world(void) {
  lock_x();
  struct world *w = cur_locked();
  unlock_x();
  return w;
}

/* The table of the fds */

enum { K_SOCKET = 1, K_PIPE_R, K_PIPE_W, K_PROC };

struct ent;

struct afd_poll {
  IO_STATUS_BLOCK iosb;
  AFD_POLL_INFO info;
  struct ent *e;
  struct world *w; /* The world of the AFD handle. */
  int dirs;        /* The directions that the poll asks for. */
  int cancelled;   /* A CancelIoEx was done: the completion starts a new poll. */
};

struct ent {
  intptr_t fd;
  int kind;
  int refs; /* The table, and each I/O that did not complete. */
  int closed;
  struct ent *next;
  struct world *w; /* The world of the registrations, or NULL. */
  uint64_t regs;   /* The domains that registered the fd. */
  int64_t tokens[64];
  /* A socket. */
  int want; /* The directions that got WSAEWOULDBLOCK since their last edge. */
  struct afd_poll *poll;
  int conn_err;
  int reset; /* An operation gave ECONNRESET. */
  /* An end of a pipe, or a child: the world of the I/O. The pointer is valid
     only while the world with this id is open. */
  struct world *assoc;
  long assoc_id;
  /* An end of a pipe: one overlapped operation at a time. */
  OVERLAPPED ov;
  int busy;
  char *buf;
  DWORD cap, len, pos;
  int eof, err;
  int close_pending; /* The close waits for the operation. */
  /* A child. */
  HANDLE packet;
  int packet_live; /* The packet waits on the process. */
};

#define NBUCKETS 4096
static struct ent *table[NBUCKETS];

static unsigned bucket(intptr_t fd) { return (unsigned)(((uintptr_t)fd >> 2) % NBUCKETS); }

/* An entry that is not closed. A pipe end that waits for its close stays in
   the table: its handle is open, so no new handle gets its value. */
static struct ent *find(intptr_t fd) {
  for (struct ent *e = table[bucket(fd)]; e != NULL; e = e->next)
    if (e->fd == fd) return e->closed ? NULL : e;
  return NULL;
}

static void insert(struct ent *e) {
  unsigned b = bucket(e->fd);
  e->next = table[b];
  table[b] = e;
}

static void remove_ent(struct ent *e) {
  struct ent **p = &table[bucket(e->fd)];
  while (*p != NULL && *p != e) p = &(*p)->next;
  if (*p == e) *p = e->next;
}

static struct ent *new_ent(intptr_t fd, int kind) {
  struct ent *e = calloc(1, sizeof *e);
  if (e == NULL) return NULL;
  e->fd = fd;
  e->kind = kind;
  e->refs = 1;
  return e;
}

/* Under the lock. */
static void unref(struct ent *e) {
  if (--e->refs == 0) {
    free(e->buf);
    free(e);
  }
}

/* The I/O of [e] is in the world of the calling thread. Under the lock. */
static int current(struct ent *e) {
  struct world *w = cur_locked();
  return w != NULL && e->assoc_id == w->id;
}

/* Send an edge to each domain that registered [e], through its port. Under
   the lock. */
static void fan_out(struct ent *e, int bits) {
  if (bits == 0 || e->w == NULL) return;
  for (int d = 0; d < e->w->n && d < 64; d++)
    if (e->regs & ((uint64_t)1 << d))
      PostQueuedCompletionStatus(e->w->ports[d].iocp, (DWORD)bits, KEY_EDGE,
                                 (LPOVERLAPPED)(intptr_t)e->tokens[d]);
}

/* Sockets */

static SOCKET base_socket(SOCKET s) {
  SOCKET b;
  DWORD n;
  if (WSAIoctl(s, SIO_BASE_HANDLE, NULL, 0, &b, sizeof b, &n, NULL, NULL) != 0) return s;
  return b;
}

/* Start a poll for the directions of [want], if no poll asks for them. A
   poll that does not ask for all of them gets a cancel, and its completion
   starts the next poll: one poll at a time. The poll goes to the world of
   the registrations, or to the world of the caller. Under the lock. */
static void start_poll(struct ent *e) {
  if (e->closed || e->want == 0) return;
  if (e->poll != NULL) {
    if ((e->poll->dirs & e->want) == e->want) return;
    if (!e->poll->cancelled) {
      e->poll->cancelled = 1;
      CancelIoEx(e->poll->w->afd, (LPOVERLAPPED)&e->poll->iosb);
    }
    return;
  }
  struct world *w = e->w != NULL ? e->w : cur_locked();
  if (w == NULL || w->closing) return;
  struct afd_poll *p = calloc(1, sizeof *p);
  if (p == NULL) return;
  ULONG mask = 0;
  if (e->want & WEFT_DIR_R) mask |= AFD_READ_EVENTS;
  if (e->want & WEFT_DIR_W) mask |= AFD_WRITE_EVENTS;
  p->e = e;
  p->w = w;
  p->dirs = e->want;
  p->info.Timeout.QuadPart = INT64_MAX;
  p->info.NumberOfHandles = 1;
  p->info.Handles[0].Handle = (HANDLE)base_socket((SOCKET)e->fd);
  p->info.Handles[0].Events = mask;
  p->iosb.Status = W_STATUS_PENDING;
  NTSTATUS st = nt_ioctl(w->afd, NULL, NULL, p, &p->iosb, IOCTL_AFD_POLL, &p->info,
                         sizeof p->info, &p->info, sizeof p->info);
  if (st != W_STATUS_PENDING && st != 0) {
    /* No completion comes. The waiters try again, and get the error. */
    free(p);
    e->want = 0;
    fan_out(e, WEFT_DIR_R | WEFT_DIR_W);
    return;
  }
  e->poll = p;
  e->refs++;
  w->outstanding++;
}

/* The completion of a poll, on the I/O thread. Under the lock. */
static void poll_done(struct afd_poll *p) {
  struct ent *e = p->e;
  p->w->outstanding--;
  if (e->poll == p) e->poll = NULL;
  int bits = 0;
  NTSTATUS st = p->iosb.Status;
  if (st == 0) {
    ULONG ev = p->info.NumberOfHandles > 0 ? p->info.Handles[0].Events : 0;
    if (ev & AFD_READ_EVENTS) bits |= WEFT_DIR_R;
    if (ev & AFD_WRITE_EVENTS) bits |= WEFT_DIR_W;
    if (ev & AFD_POLL_CONNECT_FAIL) e->conn_err = status_errno(p->info.Handles[0].Status);
  } else if (st != W_STATUS_CANCELLED) {
    bits = WEFT_DIR_R | WEFT_DIR_W;
  }
  if (!e->closed) {
    e->want &= ~bits;
    fan_out(e, bits);
    start_poll(e);
  }
  free(p);
  unref(e);
}

int weft_win_add_socket(SOCKET s) {
  struct ent *e = new_ent((intptr_t)s, K_SOCKET);
  if (e == NULL) return -ENOMEM;
  lock_x();
  insert(e);
  unlock_x();
  return 0;
}

void weft_win_arm(SOCKET s, int dir) {
  lock_x();
  struct ent *e = find((intptr_t)s);
  if (e != NULL && e->kind == K_SOCKET) {
    e->want |= dir;
    start_poll(e);
  }
  unlock_x();
}

int weft_win_connect_error(SOCKET s) {
  lock_x();
  struct ent *e = find((intptr_t)s);
  int r = e != NULL ? e->conn_err : 0;
  unlock_x();
  return r;
}

int weft_win_reset(SOCKET s) {
  lock_x();
  struct ent *e = find((intptr_t)s);
  int r = 0;
  if (e != NULL) {
    r = e->reset;
    e->reset = 1;
  }
  unlock_x();
  return r;
}

/* Pipes */

/* The close of an end of a pipe, when no operation runs. Under the lock. */
static void pipe_close_now(struct ent *e) {
  remove_ent(e);
  CloseHandle((HANDLE)e->fd);
  unref(e);
}

/* The completion of a read or a write of a pipe, on the I/O thread. Under
   the lock. */
static void pipe_done(struct ent *e, DWORD n, NTSTATUS st) {
  e->assoc->outstanding--;
  e->busy = 0;
  if (e->kind == K_PIPE_R) {
    if (st == 0) {
      e->len = n;
      e->pos = 0;
    } else if (st == W_STATUS_PIPE_BROKEN) {
      e->eof = 1;
    } else if (st != W_STATUS_CANCELLED) {
      e->err = status_errno(st);
    }
  } else if (st != 0 && st != W_STATUS_CANCELLED) {
    e->err = st == W_STATUS_PIPE_BROKEN ? EPIPE : status_errno(st);
  }
  if (e->close_pending) pipe_close_now(e);
  else fan_out(e, e->kind == K_PIPE_R ? WEFT_DIR_R : WEFT_DIR_W);
  unref(e);
}

/* An end of the world: on the I/O port of the world of the caller, with a
   buffer. A read or a write that completes at once gives its result at
   once, and no completion. */
static int add_pipe_end(HANDLE h, int kind) {
  struct world *w = cur_world();
  if (w == NULL) return -EINVAL;
  struct ent *e = new_ent((intptr_t)h, kind);
  if (e == NULL) return -ENOMEM;
  e->cap = 65536;
  e->buf = malloc(e->cap);
  if (e->buf == NULL || CreateIoCompletionPort(h, w->io_iocp, KEY_PIPE, 0) == NULL ||
      !SetFileCompletionNotificationModes(
          h, FILE_SKIP_COMPLETION_PORT_ON_SUCCESS | FILE_SKIP_SET_EVENT_ON_HANDLE)) {
    int err = e->buf == NULL ? ENOMEM : weft_win_errno(GetLastError());
    free(e->buf);
    free(e);
    return -err;
  }
  e->assoc = w;
  e->assoc_id = w->id;
  lock_x();
  insert(e);
  unlock_x();
  return 0;
}

static LONG pipe_count = 0;

/* A named pipe with one instance. The server end has overlapped I/O, and
   reads if [server_reads]. The client end has overlapped I/O if
   [client_overlapped], and a child can inherit it if [inherit]. */
static int make_pipe(HANDLE *server, HANDLE *client, int server_reads, int client_overlapped,
                     int inherit) {
  wchar_t name[96];
  swprintf(name, 96, L"\\\\.\\pipe\\weft-%lu-%ld", (unsigned long)GetCurrentProcessId(),
           (long)InterlockedIncrement(&pipe_count));
  HANDLE s = CreateNamedPipeW(
      name,
      (server_reads ? PIPE_ACCESS_INBOUND : PIPE_ACCESS_OUTBOUND) | FILE_FLAG_OVERLAPPED |
          FILE_FLAG_FIRST_PIPE_INSTANCE,
      PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 65536,
      65536, 0, NULL);
  if (s == INVALID_HANDLE_VALUE) return -weft_win_errno(GetLastError());
  SECURITY_ATTRIBUTES sa = {sizeof sa, NULL, inherit ? TRUE : FALSE};
  HANDLE c = CreateFileW(name, server_reads ? GENERIC_WRITE : GENERIC_READ | FILE_WRITE_ATTRIBUTES,
                         0, &sa, OPEN_EXISTING, client_overlapped ? FILE_FLAG_OVERLAPPED : 0,
                         NULL);
  if (c == INVALID_HANDLE_VALUE) {
    int e = weft_win_errno(GetLastError());
    CloseHandle(s);
    return -e;
  }
  *server = s;
  *client = c;
  return 0;
}

int weft_win_pipe(intptr_t *r, intptr_t *w) {
  weft_win_init();
  HANDLE s, c;
  int e = make_pipe(&s, &c, 1, 1, 0);
  if (e < 0) return e;
  if ((e = add_pipe_end(s, K_PIPE_R)) < 0) {
    CloseHandle(s);
    CloseHandle(c);
    return e;
  }
  if ((e = add_pipe_end(c, K_PIPE_W)) < 0) {
    weft_win_close((intptr_t)s);
    CloseHandle(c);
    return e;
  }
  *r = (intptr_t)s;
  *w = (intptr_t)c;
  return 0;
}

/* Start an overlapped operation of [e]. Under the lock. */
static void op_started(struct ent *e) {
  e->busy = 1;
  e->refs++;
  e->assoc->outstanding++;
}

int64_t weft_win_pipe_read(intptr_t fd, char *buf, int64_t len) {
  int64_t r = -EAGAIN;
  lock_x();
  struct ent *e = find(fd);
  if (e == NULL || e->kind != K_PIPE_R) {
    r = -EBADF;
  } else if (e->pos < e->len) {
    DWORD n = e->len - e->pos;
    if ((int64_t)n > len) n = (DWORD)len;
    memcpy(buf, e->buf + e->pos, n);
    e->pos += n;
    r = n;
  } else if (e->err) {
    r = -e->err;
  } else if (e->eof) {
    r = 0;
  } else if (len == 0) {
    r = 0;
  } else if (e->busy) {
    r = -EAGAIN;
  } else if (!current(e)) {
    r = -EBADF;
  } else {
    /* A read of 0 bytes comes from a write of 0 bytes: read again. After 8,
       an edge makes the reader try again later. */
    int i;
    for (i = 0; i < 8; i++) {
      DWORD n = 0;
      memset(&e->ov, 0, sizeof e->ov);
      if (ReadFile((HANDLE)fd, e->buf, e->cap, &n, &e->ov)) {
        if (n == 0) continue;
        DWORD k = (int64_t)n > len ? (DWORD)len : n;
        memcpy(buf, e->buf, k);
        e->len = n;
        e->pos = k;
        r = k;
        break;
      }
      DWORD err = GetLastError();
      if (err == ERROR_IO_PENDING) {
        op_started(e);
      } else if (err == ERROR_BROKEN_PIPE || err == ERROR_PIPE_NOT_CONNECTED) {
        e->eof = 1;
        r = 0;
      } else {
        e->err = weft_win_errno(err);
        r = -e->err;
      }
      break;
    }
    if (i == 8) fan_out(e, WEFT_DIR_R);
  }
  unlock_x();
  return r;
}

int64_t weft_win_pipe_write(intptr_t fd, const char *buf, int64_t len) {
  int64_t r;
  lock_x();
  struct ent *e = find(fd);
  if (e == NULL || e->kind != K_PIPE_W) {
    r = -EBADF;
  } else if (e->err) {
    r = -e->err;
  } else if (len == 0) {
    r = 0;
  } else if (e->busy) {
    r = -EAGAIN;
  } else if (!current(e)) {
    r = -EBADF;
  } else {
    DWORD n = (int64_t)e->cap < len ? e->cap : (DWORD)len, done = 0;
    memcpy(e->buf, buf, n);
    memset(&e->ov, 0, sizeof e->ov);
    if (WriteFile((HANDLE)fd, e->buf, n, &done, &e->ov)) {
      r = n;
    } else {
      DWORD err = GetLastError();
      if (err == ERROR_IO_PENDING) {
        op_started(e);
        r = n;
      } else if (err == ERROR_NO_DATA || err == ERROR_BROKEN_PIPE ||
                 err == ERROR_PIPE_NOT_CONNECTED) {
        e->err = EPIPE;
        r = -EPIPE;
      } else {
        r = -weft_win_errno(err);
      }
    }
  }
  unlock_x();
  return r;
}

/* Children */

/* The completion of the wait packet of a child: it exited. On the I/O
   thread, under the lock. */
static void proc_done(struct ent *e) {
  e->assoc->outstanding--;
  e->packet_live = 0;
  if (!e->closed) fan_out(e, WEFT_DIR_R | WEFT_DIR_W);
  unref(e);
}

/* NtCancelWaitCompletionPacket removed the packet: it gives a success, or
   STATUS_CANCELLED for a packet that was in the queue (the Go runtime takes
   the two as a removal). */
static int packet_removed(HANDLE packet) {
  NTSTATUS st = nt_cancel_packet(packet, TRUE);
  return st == 0 || st == W_STATUS_CANCELLED;
}

/* Add a child to the table, and connect its exit to the I/O port of the
   world of the caller. */
static int add_child(HANDLE h) {
  struct world *w = cur_world();
  if (w == NULL) return -EINVAL;
  struct ent *e = new_ent((intptr_t)h, K_PROC);
  if (e == NULL) return -ENOMEM;
  if (nt_create_packet(&e->packet, GENERIC_ALL, NULL) != 0) {
    free(e);
    return -ENOMEM;
  }
  BOOLEAN signaled = FALSE;
  lock_x();
  int ok = nt_associate(e->packet, w->io_iocp, h, (PVOID)KEY_PROC, e, 0, 0, &signaled) == 0;
  if (ok) {
    insert(e);
    e->assoc = w;
    e->assoc_id = w->id;
    e->packet_live = 1;
    e->refs++;
    w->outstanding++;
  }
  unlock_x();
  if (!ok) {
    CloseHandle(e->packet);
    free(e);
    return -ENOMEM;
  }
  return 0;
}

/* The close of an fd of the table */

int weft_win_close(intptr_t fd) {
  lock_x();
  struct ent *e = find(fd);
  if (e == NULL) {
    unlock_x();
    return CloseHandle((HANDLE)fd) ? 0 : -EBADF;
  }
  e->closed = 1;
  e->regs = 0;
  int r = 0;
  switch (e->kind) {
  case K_SOCKET:
    /* The close completes the poll with LOCAL_CLOSE. */
    remove_ent(e);
    if (closesocket((SOCKET)fd) != 0) r = -weft_wsa_errno(WSAGetLastError());
    unref(e);
    break;
  case K_PIPE_R:
  case K_PIPE_W:
    if (e->busy) {
      /* A write keeps its data: the close waits for it. A read stops. */
      e->close_pending = 1;
      if (e->kind == K_PIPE_R) CancelIoEx((HANDLE)fd, &e->ov);
    } else {
      pipe_close_now(e);
    }
    break;
  case K_PROC:
    remove_ent(e);
    if (e->packet_live && packet_removed(e->packet)) {
      /* No completion comes. */
      e->packet_live = 0;
      e->assoc->outstanding--;
      e->refs--;
    }
    CloseHandle(e->packet);
    CloseHandle((HANDLE)fd);
    unref(e);
    break;
  }
  unlock_x();
  return r;
}

/* The I/O thread of a world. It takes the completions of the polls, the
   pipes and the children, and it sends each edge to the domains that
   registered the fd. Thus no domain waits for the I/O of another domain. It
   runs no OCaml code. */
static DWORD WINAPI io_main(LPVOID arg) {
  struct world *w = arg;
  OVERLAPPED_ENTRY ents[MAX_EVENTS];
  for (;;) {
    ULONG n = 0;
    if (!GetQueuedCompletionStatusEx(w->io_iocp, ents, MAX_EVENTS, &n, INFINITE, FALSE))
      continue;
    int stop = 0;
    lock_x();
    for (ULONG i = 0; i < n; i++) {
      OVERLAPPED_ENTRY *x = &ents[i];
      switch (x->lpCompletionKey) {
      case KEY_AFD:
        poll_done((struct afd_poll *)x->lpOverlapped);
        break;
      case KEY_PIPE:
        pipe_done((struct ent *)((char *)x->lpOverlapped - offsetof(struct ent, ov)),
                  x->dwNumberOfBytesTransferred, (NTSTATUS)x->lpOverlapped->Internal);
        break;
      case KEY_PROC:
        proc_done((struct ent *)x->lpOverlapped);
        break;
      case KEY_STOP:
        stop = 1;
        break;
      }
    }
    unlock_x();
    if (stop) return 0;
  }
}

/* The OCaml stubs of the world (Weft_windows) */

#define World_val(v) ((struct world *)Nativeint_val(v))

static HANDLE new_timer(void) {
  HANDLE t = CreateWaitableTimerExW(NULL, NULL, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,
                                    TIMER_ALL_ACCESS);
  if (t == NULL) t = CreateWaitableTimerExW(NULL, NULL, 0, TIMER_ALL_ACCESS);
  return t;
}

static HANDLE afd_open(HANDLE iocp) {
  static WCHAR name[] = L"\\Device\\Afd\\Weft";
  UNICODE_STRING us = {sizeof name - sizeof(WCHAR), sizeof name, name};
  OBJECT_ATTRIBUTES attr;
  InitializeObjectAttributes(&attr, &us, 0, NULL, NULL);
  IO_STATUS_BLOCK iosb;
  HANDLE h;
  if (nt_create_file(&h, SYNCHRONIZE, &attr, &iosb, NULL, 0, FILE_SHARE_READ | FILE_SHARE_WRITE,
                     FILE_OPEN, 0, NULL, 0) != 0)
    return NULL;
  if (CreateIoCompletionPort(h, iocp, KEY_AFD, 0) == NULL) {
    CloseHandle(h);
    return NULL;
  }
  SetFileCompletionNotificationModes(h, FILE_SKIP_SET_EVENT_ON_HANDLE);
  return h;
}

/* After the stop of the I/O thread, or before its start. */
static void free_world(struct world *w) {
  for (int i = 0; i < w->n; i++) {
    struct port *p = &w->ports[i];
    if (p->timer_packet != NULL) CloseHandle(p->timer_packet);
    if (p->timer != NULL) CloseHandle(p->timer);
    if (p->iocp != NULL) CloseHandle(p->iocp);
  }
  if (w->afd != NULL) CloseHandle(w->afd);
  if (w->io_thread != NULL) CloseHandle(w->io_thread);
  if (w->io_iocp != NULL) CloseHandle(w->io_iocp);
  free(w);
}

/* [weft_win_create n] makes the ports of [n] domains and the I/O thread. The
   world becomes the world of the calling thread. */
value weft_win_create(value vn) {
  weft_win_init();
  int n = Int_val(vn);
  struct world *w = calloc(1, sizeof *w + n * sizeof(struct port));
  if (w == NULL) caml_raise_out_of_memory();
  w->n = n;
  int ok = nt_create_file && nt_ioctl && nt_create_packet && nt_associate && nt_cancel_packet;
  for (int i = 0; ok && i < n; i++) {
    struct port *p = &w->ports[i];
    p->iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
    p->timer = new_timer();
    if (p->iocp == NULL || p->timer == NULL ||
        nt_create_packet(&p->timer_packet, GENERIC_ALL, NULL) != 0)
      ok = 0;
  }
  if (ok) ok = (w->io_iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0)) != NULL;
  if (ok) ok = (w->afd = afd_open(w->io_iocp)) != NULL;
  if (ok) ok = (w->io_thread = CreateThread(NULL, 0, io_main, w, 0, NULL)) != NULL;
  if (!ok) {
    free_world(w);
    caml_failwith("Weft_windows.create: the ports of the domains");
  }
  lock_x();
  w->id = ++world_ids;
  w->next = worlds;
  worlds = w;
  unlock_x();
  tls_world = w;
  tls_id = w->id;
  return caml_copy_nativeint((intnat)w);
}

/* The IOCP of each domain, for the mailboxes of the thread pools. */
value weft_win_iocps(value vw) {
  CAMLparam1(vw);
  CAMLlocal1(r);
  struct world *w = World_val(vw);
  r = caml_alloc_tuple(w->n);
  for (int i = 0; i < w->n; i++) Store_field(r, i, Val_long((intnat)w->ports[i].iocp));
  CAMLreturn(r);
}

/* After the domains end. The world stops its I/O, and the I/O thread takes
   the completions of the cancels. Then the thread stops, and the ports
   close. An end of a pipe of the world stays open, but it gives EBADF: its
   handle stays on the closed port. If the I/O does not end in 10 s, the
   world and its thread stay in memory: an entry can point to it. */
value weft_win_close_world(value vw) {
  struct world *w = World_val(vw);
  lock_x();
  struct world **p = &worlds;
  while (*p != NULL && *p != w) p = &(*p)->next;
  if (*p == w) *p = w->next;
  w->closing = 1;
  for (int b = 0; b < NBUCKETS; b++)
    for (struct ent *e = table[b]; e != NULL; e = e->next) {
      if (e->w == w) {
        e->w = NULL;
        e->regs = 0;
      }
      if (e->kind == K_SOCKET && e->poll != NULL && e->poll->w == w)
        CancelIoEx(w->afd, (LPOVERLAPPED)&e->poll->iosb);
      if (e->assoc_id != w->id) continue;
      if ((e->kind == K_PIPE_R || e->kind == K_PIPE_W) && e->busy)
        CancelIoEx((HANDLE)e->fd, &e->ov);
      if (e->kind == K_PROC && e->packet_live && packet_removed(e->packet)) {
        e->packet_live = 0;
        w->outstanding--;
        e->refs--;
      }
    }
  unlock_x();
  if (tls_world == w) tls_world = NULL;
  int left = 1;
  caml_release_runtime_system();
  for (int i = 0; i < 1000; i++) {
    lock_x();
    left = w->outstanding;
    unlock_x();
    if (left <= 0) break;
    Sleep(10);
  }
  if (left <= 0) {
    PostQueuedCompletionStatus(w->io_iocp, 0, KEY_STOP, NULL);
    WaitForSingleObject(w->io_thread, INFINITE);
    free_world(w);
  }
  caml_acquire_runtime_system();
  return Val_unit;
}

value weft_win_register(value vw, value vd, value vfd, value vtoken) {
  struct world *w = World_val(vw);
  int d = Int_val(vd);
  int r = 0;
  lock_x();
  struct ent *e = find(Long_val(vfd));
  if (e == NULL) {
    r = -EPERM;
  } else if (d >= 64) {
    r = -EINVAL;
  } else {
    e->w = w;
    e->regs |= (uint64_t)1 << d;
    e->tokens[d] = Long_val(vtoken);
    /* One edge at once, as an EPOLL_CTL_ADD of a ready fd. */
    PostQueuedCompletionStatus(w->ports[d].iocp, WEFT_DIR_R | WEFT_DIR_W, KEY_EDGE,
                               (LPOVERLAPPED)(intptr_t)e->tokens[d]);
  }
  unlock_x();
  return Val_int(r);
}

value weft_win_deregister(value vw, value vd, value vfd) {
  int d = Int_val(vd);
  int r = 0;
  lock_x();
  struct ent *e = find(Long_val(vfd));
  if (e == NULL) r = -EBADF;
  else if (d < 64) e->regs &= ~((uint64_t)1 << d);
  unlock_x();
  return Val_int(r);
}

value weft_win_wake(value vw, value vd) {
  struct world *w = World_val(vw);
  PostQueuedCompletionStatus(w->ports[Int_val(vd)].iocp, 0, WEFT_KEY_WAKE, NULL);
  return Val_unit;
}

static value edge(int64_t token, int read, int write) {
  value ev = caml_alloc_small(3, 0);
  Field(ev, 0) = Val_long(token);
  Field(ev, 1) = Val_bool(read);
  Field(ev, 2) = Val_bool(write);
  return ev;
}

/* [weft_win_wait w d timeout_ns]: [timeout_ns] is 0 for a poll, -1 for no
   timeout, or the time of the alarm from now. It gives (token, read, write)
   for each edge, with the token -1 for the alarm and -2 for a wake, as the
   Linux world. If the timer cannot reach the port, the wait has a timeout in
   milliseconds. */
value weft_win_wait(value vw, value vd, value vtimeout) {
  CAMLparam3(vw, vd, vtimeout);
  CAMLlocal2(res, ev);
  struct world *w = World_val(vw);
  struct port *p = &w->ports[Int_val(vd)];
  int64_t t = Int64_val(vtimeout);
  DWORD ms = t == 0 ? 0 : INFINITE;
  tls_world = w;
  tls_id = w->id;
  if (t > 0) {
    LARGE_INTEGER due;
    /* Relative, in units of 100 ns. */
    due.QuadPart = -((t + 99) / 100);
    SetWaitableTimer(p->timer, &due, 0, NULL, NULL, FALSE);
    if (!p->timer_assoc) {
      BOOLEAN signaled = FALSE;
      if (nt_associate(p->timer_packet, p->iocp, p->timer, (PVOID)KEY_TIMER, NULL, 0, 0,
                       &signaled) == 0)
        p->timer_assoc = 1;
    }
    if (!p->timer_assoc) ms = (DWORD)((t + 999999) / 1000000);
  } else if (t < 0) {
    CancelWaitableTimer(p->timer);
  }
  OVERLAPPED_ENTRY ents[MAX_EVENTS];
  ULONG n = 0;
  caml_release_runtime_system();
  if (!GetQueuedCompletionStatusEx(p->iocp, ents, MAX_EVENTS, &n, ms, FALSE)) n = 0;
  caml_acquire_runtime_system();
  int alarm = t > 0 && n == 0 && !p->timer_assoc, woken = 0, edges = 0;
  for (ULONG i = 0; i < n; i++) {
    if (ents[i].lpCompletionKey == KEY_TIMER) {
      p->timer_assoc = 0;
      alarm = 1;
    } else if (ents[i].lpCompletionKey == WEFT_KEY_WAKE) {
      woken = 1;
    } else if (ents[i].lpCompletionKey == KEY_EDGE) {
      edges++;
    }
  }
  res = caml_alloc_tuple(edges + alarm + woken);
  int k = 0;
  if (alarm) {
    ev = edge(-1, 0, 0);
    Store_field(res, k++, ev);
  }
  if (woken) {
    ev = edge(-2, 0, 0);
    Store_field(res, k++, ev);
  }
  for (ULONG i = 0; i < n; i++) {
    if (ents[i].lpCompletionKey != KEY_EDGE) continue;
    DWORD bits = ents[i].dwNumberOfBytesTransferred;
    ev = edge((int64_t)(intptr_t)ents[i].lpOverlapped, bits & WEFT_DIR_R, bits & WEFT_DIR_W);
    Store_field(res, k++, ev);
  }
  CAMLreturn(res);
}

/* Children */

/* A copy of a handle that a child can inherit. */
static HANDLE inheritable(HANDLE h) {
  HANDLE c = NULL;
  if (h == NULL || h == INVALID_HANDLE_VALUE) return NULL;
  if (!DuplicateHandle(GetCurrentProcess(), h, GetCurrentProcess(), &c, 0, TRUE,
                       DUPLICATE_SAME_ACCESS))
    return NULL;
  return c;
}

/* Add the quoted form of [arg] to [out], with the rules of the C runtime of
   Microsoft (CommandLineToArgvW). [out] has room for 2 * len + 3 chars. */
static wchar_t *quote_arg(wchar_t *out, const wchar_t *arg) {
  int needs = arg[0] == 0 || wcspbrk(arg, L" \t\n\v\"") != NULL;
  if (!needs) {
    wcscpy(out, arg);
    return out + wcslen(arg);
  }
  *out++ = L'"';
  for (const wchar_t *p = arg;; p++) {
    int slashes = 0;
    while (*p == L'\\') {
      p++;
      slashes++;
    }
    if (*p == 0) {
      for (int i = 0; i < 2 * slashes; i++) *out++ = L'\\';
      break;
    }
    if (*p == L'"') {
      for (int i = 0; i < 2 * slashes + 1; i++) *out++ = L'\\';
      *out++ = L'"';
    } else {
      for (int i = 0; i < slashes; i++) *out++ = L'\\';
      *out++ = *p;
    }
  }
  *out++ = L'"';
  return out;
}

/* The path of [prog]: a name with no separator is found in the PATH, with
   ".exe" if it has no extension. */
static wchar_t *find_program(const wchar_t *prog) {
  if (wcspbrk(prog, L"/\\:") != NULL) {
    DWORD a = GetFileAttributesW(prog);
    if (a != INVALID_FILE_ATTRIBUTES && !(a & FILE_ATTRIBUTE_DIRECTORY)) return _wcsdup(prog);
    size_t n = wcslen(prog);
    wchar_t *x = malloc((n + 5) * sizeof *x);
    if (x == NULL) return NULL;
    wcscpy(x, prog);
    wcscat(x, L".exe");
    a = GetFileAttributesW(x);
    if (a != INVALID_FILE_ATTRIBUTES && !(a & FILE_ATTRIBUTE_DIRECTORY)) return x;
    free(x);
    return NULL;
  }
  DWORD n = SearchPathW(NULL, prog, L".exe", 0, NULL, NULL);
  if (n == 0) return NULL;
  wchar_t *x = malloc((n + 1) * sizeof *x);
  if (x == NULL) return NULL;
  if (SearchPathW(NULL, prog, L".exe", n + 1, x, NULL) == 0) {
    free(x);
    return NULL;
  }
  return x;
}

/* [weft_spawn args defaults], as on POSIX. [args] is (prog, argv, env,
   has_env, cwd, stdio): stdio has a kind for the fds 0, 1 and 2 (0 inherit,
   1 NUL, 2 a pipe). [defaults] has no use on Windows. The result is (pid,
   pidfd, stdin, stdout, stderr): pidfd is the handle of the process, and the
   others are the ends of the world, or -1. pid is -errno on an error. */
value weft_spawn(value args, value defaults) {
  CAMLparam2(args, defaults);
  CAMLlocal1(res);
  weft_win_init();
  int e = 0;
  intptr_t ends[3] = {-1, -1, -1};
  HANDLE child[3] = {NULL, NULL, NULL};
  wchar_t *prog = NULL, *path = NULL, *cmd = NULL, *envb = NULL, *cwd = NULL;
  PROCESS_INFORMATION pi = {0};
  LPPROC_THREAD_ATTRIBUTE_LIST attrs = NULL;
  value argv = Field(args, 1), env = Field(args, 2), stdio = Field(args, 5);

  if (cur_world() == NULL) {
    e = EINVAL;
    goto done;
  }
  prog = weft_wide(String_val(Field(args, 0)));
  if (prog == NULL) {
    e = EINVAL;
    goto done;
  }
  path = find_program(prog);
  if (path == NULL) {
    e = ENOENT;
    goto done;
  }
  /* The command line. */
  size_t cap = 1;
  for (mlsize_t i = 0; i < Wosize_val(argv); i++)
    cap += 2 * caml_string_length(Field(argv, i)) + 4;
  cmd = malloc(cap * sizeof *cmd);
  if (cmd == NULL) {
    e = ENOMEM;
    goto done;
  }
  wchar_t *q = cmd;
  for (mlsize_t i = 0; i < Wosize_val(argv); i++) {
    wchar_t *a = weft_wide(String_val(Field(argv, i)));
    if (a == NULL) {
      e = EINVAL;
      goto done;
    }
    if (i > 0) *q++ = L' ';
    q = quote_arg(q, a);
    free(a);
  }
  *q = 0;
  /* The environment: each "name=value", then one more NUL. */
  if (Bool_val(Field(args, 3))) {
    size_t total = 1;
    for (mlsize_t i = 0; i < Wosize_val(env); i++)
      total += caml_string_length(Field(env, i)) + 1;
    envb = malloc(total * sizeof *envb);
    if (envb == NULL) {
      e = ENOMEM;
      goto done;
    }
    wchar_t *p = envb;
    for (mlsize_t i = 0; i < Wosize_val(env); i++) {
      wchar_t *a = weft_wide(String_val(Field(env, i)));
      if (a == NULL) {
        e = EINVAL;
        goto done;
      }
      wcscpy(p, a);
      p += wcslen(a) + 1;
      free(a);
    }
    *p = 0;
  }
  if (caml_string_length(Field(args, 4)) > 0) {
    cwd = weft_wide(String_val(Field(args, 4)));
    DWORD a = cwd == NULL ? INVALID_FILE_ATTRIBUTES : GetFileAttributesW(cwd);
    if (a == INVALID_FILE_ATTRIBUTES) {
      e = ENOENT;
      goto done;
    }
    if (!(a & FILE_ATTRIBUTE_DIRECTORY)) {
      e = ENOTDIR;
      goto done;
    }
  }
  /* The handles of the child. */
  static const DWORD stds[3] = {STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE};
  for (int i = 0; i < 3 && e == 0; i++) {
    int kind = Int_val(Field(stdio, i));
    if (kind == 0) {
      child[i] = inheritable(GetStdHandle(stds[i]));
    } else if (kind == 1) {
      SECURITY_ATTRIBUTES sa = {sizeof sa, NULL, TRUE};
      child[i] = CreateFileW(L"NUL", GENERIC_READ | GENERIC_WRITE,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, &sa, OPEN_EXISTING, 0, NULL);
      if (child[i] == INVALID_HANDLE_VALUE) {
        child[i] = NULL;
        e = weft_win_errno(GetLastError());
      }
    } else {
      /* The child reads fd 0 and writes the fds 1 and 2. */
      HANDLE s, c;
      int r = make_pipe(&s, &c, i != 0, 0, 1);
      if (r < 0) {
        e = -r;
      } else if ((r = add_pipe_end(s, i == 0 ? K_PIPE_W : K_PIPE_R)) < 0) {
        CloseHandle(s);
        CloseHandle(c);
        e = -r;
      } else {
        ends[i] = (intptr_t)s;
        child[i] = c;
      }
    }
  }
  if (e != 0) goto done;
  /* The child inherits only these handles. A NULL handle is not in the
     list. */
  HANDLE list[3];
  int nlist = 0;
  for (int i = 0; i < 3; i++)
    if (child[i] != NULL) list[nlist++] = child[i];
  SIZE_T size = 0;
  InitializeProcThreadAttributeList(NULL, 1, 0, &size);
  attrs = malloc(size);
  if (attrs == NULL || !InitializeProcThreadAttributeList(attrs, 1, 0, &size)) {
    free(attrs);
    attrs = NULL;
    e = ENOMEM;
    goto done;
  }
  if (nlist > 0)
    UpdateProcThreadAttribute(attrs, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, list,
                              nlist * sizeof(HANDLE), NULL, NULL);
  STARTUPINFOEXW si;
  memset(&si, 0, sizeof si);
  si.StartupInfo.cb = sizeof si;
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput = child[0];
  si.StartupInfo.hStdOutput = child[1];
  si.StartupInfo.hStdError = child[2];
  si.lpAttributeList = attrs;
  if (!CreateProcessW(path, cmd, NULL, NULL, nlist > 0,
                      EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT, envb, cwd,
                      &si.StartupInfo, &pi)) {
    e = weft_win_errno(GetLastError());
    goto done;
  }
  CloseHandle(pi.hThread);
  int r = add_child(pi.hProcess);
  if (r < 0) {
    TerminateProcess(pi.hProcess, 1);
    CloseHandle(pi.hProcess);
    e = -r;
  }
done:
  if (attrs != NULL) {
    DeleteProcThreadAttributeList(attrs);
    free(attrs);
  }
  for (int i = 0; i < 3; i++) {
    if (child[i] != NULL) CloseHandle(child[i]);
    if (e != 0 && ends[i] != -1) {
      weft_win_close(ends[i]);
      ends[i] = -1;
    }
  }
  free(prog);
  free(path);
  free(cmd);
  free(envb);
  free(cwd);
  res = caml_alloc_tuple(5);
  Store_field(res, 0, Val_long(e != 0 ? -e : (intnat)pi.dwProcessId));
  Store_field(res, 1, Val_long(e != 0 ? -1 : (intnat)pi.hProcess));
  for (int i = 0; i < 3; i++) Store_field(res, 2 + i, Val_long(ends[i]));
  CAMLreturn(res);
}


/* (kind, code): kind 0 while the child runs, 1 if it exited with the code,
   or -errno. */
value weft_win_wait_child(value vfd) {
  CAMLparam1(vfd);
  CAMLlocal1(r);
  int kind = -EBADF;
  DWORD code = 0;
  lock_x();
  struct ent *e = find(Long_val(vfd));
  if (e != NULL && e->kind == K_PROC) {
    if (WaitForSingleObject((HANDLE)e->fd, 0) == WAIT_OBJECT_0) {
      GetExitCodeProcess((HANDLE)e->fd, &code);
      kind = 1;
    } else {
      kind = 0;
    }
  }
  unlock_x();
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, Val_int(code));
  CAMLreturn(r);
}

/* Each signal ends the child, with the exit code 1. A child that exited
   gives no error, also if it exits during the call. */
value weft_win_kill(value vfd) {
  int r = 0;
  lock_x();
  struct ent *e = find(Long_val(vfd));
  if (e == NULL || e->kind != K_PROC) r = -EBADF;
  else if (!TerminateProcess((HANDLE)e->fd, 1) &&
           WaitForSingleObject((HANDLE)e->fd, 0) != WAIT_OBJECT_0)
    r = -weft_win_errno(GetLastError());
  unlock_x();
  return Val_int(r);
}

/* Ctrl-C (design section 2.7, Windows). Windows has no signals. The console
   gives its events to a handler on a thread of the system: Ctrl-C is SIGINT,
   Ctrl-Break is SIGBREAK, and the close of the console, a logoff or a
   shutdown is SIGHUP. The handler counts a watched event and wakes domain 0,
   as the handler of the kqueue world does. A handler holds a shared lock,
   and a change of the watch takes the lock: after the change, no handler
   uses the old watch or the old port.

   Windows ends the process when the handler of a close, a logoff or a
   shutdown returns. Thus that handler blocks after its wake, as the handler
   of the Go runtime does: the fibers get the time to end the program, and
   Windows ends the process after its limit (5 s for a close). */

#define NSIG_WIN 3
static const int sig_nums[NSIG_WIN] = {2 /* SIGINT */, 21 /* SIGBREAK */, 1 /* SIGHUP */};
static volatile LONG sig_counts[NSIG_WIN];
static int sig_watched;
static HANDLE sig_iocp;
static int handler_set = 0;
static SRWLOCK sig_lock = SRWLOCK_INIT;

static BOOL WINAPI ctrl_handler(DWORD ev) {
  int i;
  switch (ev) {
  case CTRL_C_EVENT:
    i = 0;
    break;
  case CTRL_BREAK_EVENT:
    i = 1;
    break;
  case CTRL_CLOSE_EVENT:
  case CTRL_LOGOFF_EVENT:
  case CTRL_SHUTDOWN_EVENT:
    i = 2;
    break;
  default:
    return FALSE;
  }
  AcquireSRWLockShared(&sig_lock);
  int taken = (sig_watched & (1 << i)) && sig_iocp != NULL;
  if (taken) {
    InterlockedIncrement(&sig_counts[i]);
    PostQueuedCompletionStatus(sig_iocp, 0, WEFT_KEY_WAKE, NULL);
  }
  ReleaseSRWLockShared(&sig_lock);
  if (taken && i == 2) Sleep(INFINITE);
  return taken;
}

static int sig_index(int num) {
  for (int i = 0; i < NSIG_WIN; i++)
    if (sig_nums[i] == num) return i;
  return -1;
}

/* [weft_win_watch w nums] watches the events of these signal numbers, and
   wakes domain 0 of [w]. The other numbers never come. */
value weft_win_watch(value vw, value nums) {
  struct world *w = World_val(vw);
  int mask = 0;
  for (mlsize_t i = 0; i < Wosize_val(nums); i++) {
    int k = sig_index(Int_val(Field(nums, i)));
    if (k >= 0) mask |= 1 << k;
  }
  AcquireSRWLockExclusive(&sig_lock);
  for (int i = 0; i < NSIG_WIN; i++) sig_counts[i] = 0;
  sig_iocp = w->ports[0].iocp;
  sig_watched = mask;
  if (!handler_set) {
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
    handler_set = 1;
  }
  ReleaseSRWLockExclusive(&sig_lock);
  return Val_unit;
}

/* The end of the watch of one number: its event gets its default action. */
value weft_win_unwatch(value num) {
  int k = sig_index(Int_val(num));
  AcquireSRWLockExclusive(&sig_lock);
  if (k >= 0) sig_watched &= ~(1 << k);
  ReleaseSRWLockExclusive(&sig_lock);
  return Val_unit;
}

/* The end of all watches, before the close of the ports of the world. */
value weft_win_unwatch_all(value unit) {
  AcquireSRWLockExclusive(&sig_lock);
  sig_watched = 0;
  sig_iocp = NULL;
  if (handler_set) {
    SetConsoleCtrlHandler(ctrl_handler, FALSE);
    handler_set = 0;
  }
  ReleaseSRWLockExclusive(&sig_lock);
  return Val_unit;
}

/* For the tests: a call of the handler, as Windows does on a thread of the
   system. A real Ctrl-C goes to each process of the console. */
value weft_win_test_ctrl(value ev) { return Val_bool(ctrl_handler((DWORD)Int_val(ev))); }

/* The counts since the last take, by signal number: (num, count) for each
   count above 0. */
value weft_win_signal_take(value unit) {
  CAMLparam0();
  CAMLlocal2(r, x);
  int nums[NSIG_WIN], counts[NSIG_WIN], k = 0;
  for (int i = 0; i < NSIG_WIN; i++) {
    if (sig_counts[i] == 0) continue;
    LONG c = InterlockedExchange(&sig_counts[i], 0);
    if (c > 0) {
      nums[k] = sig_nums[i];
      counts[k] = c;
      k++;
    }
  }
  r = caml_alloc_tuple(k);
  for (int i = 0; i < k; i++) {
    x = caml_alloc_tuple(2);
    Store_field(x, 0, Val_int(nums[i]));
    Store_field(x, 1, Val_int(counts[i]));
    Store_field(r, i, x);
  }
  CAMLreturn(r);
}

#endif
