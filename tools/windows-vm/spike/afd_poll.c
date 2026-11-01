/* The spike of AFD_POLL for the Windows world (plan step 1). Each case prints
   one line with OK or FAIL. The program uses the definitions of wepoll and
   mio: AFD_POLL is an IOCTL of the afd.sys driver, and Microsoft does not
   document it.

   The questions:
   - Does a poll of a ready socket complete at once? (The edge rule of the
     simulator: a Register of a ready fd gives one edge at once.)
   - Does a poll after WSAEWOULDBLOCK wait for the next edge?
   - Do two polls on two IOCPs (two domains) both complete? No: only the
     oldest poll of a socket gets its events (case 6).
   - Do a cancel, a close of the peer, a local close, an accept and a failed
     connect complete a poll with the correct event?

   Build in the VM: x86_64-w64-mingw32-gcc -O2 -o afd_poll.exe afd_poll.c
   -lws2_32 */

#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <winternl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define IOCTL_AFD_POLL 0x00012024
#define AFD_POLL_RECEIVE 0x0001
#define AFD_POLL_SEND 0x0004
#define AFD_POLL_DISCONNECT 0x0008
#define AFD_POLL_ABORT 0x0010
#define AFD_POLL_LOCAL_CLOSE 0x0020
#define AFD_POLL_ACCEPT 0x0080
#define AFD_POLL_CONNECT_FAIL 0x0100
#define AFD_ALL                                                         \
  (AFD_POLL_RECEIVE | AFD_POLL_SEND | AFD_POLL_DISCONNECT | AFD_POLL_ABORT | \
   AFD_POLL_LOCAL_CLOSE | AFD_POLL_ACCEPT | AFD_POLL_CONNECT_FAIL)
#define AFD_READ                                                     \
  (AFD_POLL_RECEIVE | AFD_POLL_DISCONNECT | AFD_POLL_ABORT |         \
   AFD_POLL_LOCAL_CLOSE | AFD_POLL_ACCEPT | AFD_POLL_CONNECT_FAIL)
#ifndef SIO_BASE_HANDLE
#define SIO_BASE_HANDLE _WSAIOR(IOC_WS2, 34)
#endif
#ifndef STATUS_CANCELLED
#define STATUS_CANCELLED ((NTSTATUS)0xC0000120L)
#endif

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

typedef NTSTATUS(NTAPI *nt_create_file_t)(PHANDLE, ACCESS_MASK,
                                          POBJECT_ATTRIBUTES,
                                          PIO_STATUS_BLOCK, PLARGE_INTEGER,
                                          ULONG, ULONG, ULONG, ULONG, PVOID,
                                          ULONG);
typedef NTSTATUS(NTAPI *nt_ioctl_t)(HANDLE, HANDLE, PVOID, PVOID,
                                    PIO_STATUS_BLOCK, ULONG, PVOID, ULONG,
                                    PVOID, ULONG);

static nt_create_file_t nt_create_file;
static nt_ioctl_t nt_ioctl;
static int failures;

/* One poll in flight: its status block and its buffer must stay valid until
   the completion. */
typedef struct {
  IO_STATUS_BLOCK iosb;
  AFD_POLL_INFO info;
} poll_t;

static void check(int ok, const char *what) {
  printf("%s  %s\n", ok ? "OK  " : "FAIL", what);
  if (!ok) failures++;
}

static HANDLE afd_open(HANDLE iocp) {
  static WCHAR name[] = L"\\Device\\Afd\\Weft";
  UNICODE_STRING us = {sizeof(name) - sizeof(WCHAR), sizeof(name), name};
  OBJECT_ATTRIBUTES attr;
  InitializeObjectAttributes(&attr, &us, 0, NULL, NULL);
  IO_STATUS_BLOCK iosb;
  HANDLE h;
  NTSTATUS st = nt_create_file(&h, SYNCHRONIZE, &attr, &iosb, NULL, 0,
                               FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN,
                               0, NULL, 0);
  if (st != 0) {
    printf("FAIL  NtCreateFile \\Device\\Afd: 0x%lx\n", (unsigned long)st);
    exit(1);
  }
  CreateIoCompletionPort(h, iocp, 0, 0);
  SetFileCompletionNotificationModes(h, FILE_SKIP_SET_EVENT_ON_HANDLE);
  return h;
}

static SOCKET base_of(SOCKET s) {
  SOCKET b;
  DWORD n;
  if (WSAIoctl(s, SIO_BASE_HANDLE, NULL, 0, &b, sizeof(b), &n, NULL, NULL))
    return s;
  return b;
}

/* Starts a poll. The ApcContext is the poll, so the completion entry gives
   it back as lpOverlapped. */
static NTSTATUS poll_start(HANDLE afd, poll_t *p, SOCKET s, ULONG events) {
  memset(p, 0, sizeof(*p));
  p->info.Timeout.QuadPart = INT64_MAX;
  p->info.NumberOfHandles = 1;
  p->info.Handles[0].Handle = (HANDLE)base_of(s);
  p->info.Handles[0].Events = events;
  p->iosb.Status = STATUS_PENDING;
  return nt_ioctl(afd, NULL, NULL, p, &p->iosb, IOCTL_AFD_POLL, &p->info,
                  sizeof(p->info), &p->info, sizeof(p->info));
}

/* Waits up to [ms] for one completion. Gives the poll, or NULL. */
static poll_t *wait_one(HANDLE iocp, DWORD ms) {
  OVERLAPPED_ENTRY e;
  ULONG n = 0;
  if (!GetQueuedCompletionStatusEx(iocp, &e, 1, &n, ms, FALSE) || n == 0)
    return NULL;
  return (poll_t *)e.lpOverlapped;
}

static void nonblock(SOCKET s) {
  u_long one = 1;
  ioctlsocket(s, FIONBIO, &one);
}

/* A connected TCP pair on 127.0.0.1, and the listener. */
static void tcp_pair(SOCKET *l, SOCKET *c, SOCKET *s) {
  struct sockaddr_in a = {0};
  int len = sizeof(a);
  a.sin_family = AF_INET;
  a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  *l = socket(AF_INET, SOCK_STREAM, 0);
  bind(*l, (struct sockaddr *)&a, sizeof(a));
  listen(*l, 16);
  getsockname(*l, (struct sockaddr *)&a, &len);
  *c = socket(AF_INET, SOCK_STREAM, 0);
  connect(*c, (struct sockaddr *)&a, sizeof(a));
  *s = accept(*l, NULL, NULL);
  nonblock(*c);
  nonblock(*s);
  nonblock(*l);
}

static void drain(SOCKET s) {
  char buf[4096];
  while (recv(s, buf, sizeof(buf), 0) > 0) {
  }
}

int main(void) {
  WSADATA wsa;
  WSAStartup(MAKEWORD(2, 2), &wsa);
  HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
  nt_create_file = (nt_create_file_t)GetProcAddress(ntdll, "NtCreateFile");
  nt_ioctl = (nt_ioctl_t)GetProcAddress(ntdll, "NtDeviceIoControlFile");

  HANDLE iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
  HANDLE afd = afd_open(iocp);
  SOCKET l, c, s;
  tcp_pair(&l, &c, &s);
  poll_t p, q;
  poll_t *r;

  /* 1. A new socket can send: the poll completes at once. */
  poll_start(afd, &p, s, AFD_ALL);
  r = wait_one(iocp, 0);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_SEND),
        "a poll of a writable socket completes at once with SEND");

  /* 2. No data: a read poll waits. */
  poll_start(afd, &p, s, AFD_READ);
  check(wait_one(iocp, 100) == NULL, "a read poll with no data waits");

  /* 3. Data from the peer completes it. */
  send(c, "x", 1, 0);
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_RECEIVE),
        "data from the peer completes the read poll with RECEIVE");

  /* 4. The data is still there: a new poll completes at once (level). The
     world thus starts a poll only after WSAEWOULDBLOCK. */
  poll_start(afd, &p, s, AFD_READ);
  r = wait_one(iocp, 0);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_RECEIVE),
        "a poll of a readable socket completes at once (one edge at once)");

  /* 5. After a read until WSAEWOULDBLOCK, the poll waits for new data. */
  drain(s);
  check(WSAGetLastError() == WSAEWOULDBLOCK, "the drain ends at WSAEWOULDBLOCK");
  poll_start(afd, &p, s, AFD_READ);
  check(wait_one(iocp, 100) == NULL, "after WSAEWOULDBLOCK, the poll waits");
  send(c, "y", 1, 0);
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_RECEIVE),
        "the next data completes the poll (the next edge)");
  drain(s);

  /* 6. Two domains: two AFD handles on two IOCPs poll the same socket. Only
     the oldest poll gets the events. The second poll does not complete, also
     after the first one completed and a new edge came. Thus the Windows world
     keeps at most one poll for each socket. */
  HANDLE iocp2 = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
  HANDLE afd2 = afd_open(iocp2);
  poll_start(afd, &p, s, AFD_READ);
  poll_start(afd2, &q, s, AFD_READ);
  send(c, "z", 1, 0);
  check(wait_one(iocp, 1000) == &p, "of two polls, the oldest completes");
  check(wait_one(iocp2, 300) == NULL, "of two polls, the second waits");
  drain(s);
  send(c, "w", 1, 0);
  check(wait_one(iocp2, 300) == NULL,
        "the second poll also misses the next edge");
  CancelIoEx(afd2, (LPOVERLAPPED)&q.iosb);
  check(wait_one(iocp2, 1000) == &q && q.iosb.Status == STATUS_CANCELLED,
        "a cancel ends the second poll");
  drain(s);

  /* 6b. The order of the start decides, not the IOCP. */
  poll_start(afd2, &q, s, AFD_READ);
  poll_start(afd, &p, s, AFD_READ);
  send(c, "v", 1, 0);
  check(wait_one(iocp2, 1000) == &q && wait_one(iocp, 300) == NULL,
        "with the other order, the other oldest poll completes");
  CancelIoEx(afd, (LPOVERLAPPED)&p.iosb);
  wait_one(iocp, 1000);
  drain(s);

  /* 6c. The same on one AFD handle. */
  poll_start(afd, &p, s, AFD_READ);
  poll_start(afd, &q, s, AFD_READ);
  send(c, "u", 1, 0);
  check(wait_one(iocp, 1000) == &p && wait_one(iocp, 300) == NULL,
        "on one AFD handle, also only the oldest poll completes");
  CancelIoEx(afd, (LPOVERLAPPED)&q.iosb);
  wait_one(iocp, 1000);
  drain(s);

  /* 7. A cancel completes the poll with STATUS_CANCELLED. */
  poll_start(afd, &p, s, AFD_READ);
  CancelIoEx(afd, (LPOVERLAPPED)&p.iosb);
  r = wait_one(iocp, 1000);
  check(r == &p && p.iosb.Status == STATUS_CANCELLED,
        "CancelIoEx completes the poll with STATUS_CANCELLED");

  /* 8. A close of the peer completes a read poll with DISCONNECT. */
  poll_start(afd, &p, s, AFD_READ);
  closesocket(c);
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_DISCONNECT),
        "a close of the peer completes the poll with DISCONNECT");

  /* 9. A local close completes a pending poll with LOCAL_CLOSE. */
  SOCKET l2, c2, s2;
  tcp_pair(&l2, &c2, &s2);
  poll_start(afd, &p, s2, AFD_READ);
  closesocket(s2);
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_LOCAL_CLOSE),
        "a local close completes the poll with LOCAL_CLOSE");
  closesocket(c2);
  closesocket(l2);

  /* 10. A connect to the listener completes its poll with ACCEPT. */
  poll_start(afd, &p, l, AFD_READ);
  check(wait_one(iocp, 100) == NULL, "a listener poll with no client waits");
  struct sockaddr_in a = {0};
  int len = sizeof(a);
  getsockname(l, (struct sockaddr *)&a, &len);
  SOCKET c3 = socket(AF_INET, SOCK_STREAM, 0);
  nonblock(c3);
  connect(c3, (struct sockaddr *)&a, sizeof(a));
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_ACCEPT),
        "a client completes the listener poll with ACCEPT");

  /* 11. A connect to a closed port: CONNECT_FAIL. Windows tries a refused
     connect again for about 2 s. */
  closesocket(l);
  SOCKET c4 = socket(AF_INET, SOCK_STREAM, 0);
  nonblock(c4);
  connect(c4, (struct sockaddr *)&a, sizeof(a));
  poll_start(afd, &p, c4, AFD_ALL);
  DWORD t0 = GetTickCount();
  r = wait_one(iocp, 5000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_CONNECT_FAIL),
        "a refused connect completes the poll with CONNECT_FAIL");
  printf("      (the refused connect took %lu ms)\n",
         (unsigned long)(GetTickCount() - t0));

  /* 12. UDP: a datagram completes a read poll. */
  SOCKET u = socket(AF_INET, SOCK_DGRAM, 0);
  struct sockaddr_in ua = {0};
  int ulen = sizeof(ua);
  ua.sin_family = AF_INET;
  ua.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  bind(u, (struct sockaddr *)&ua, sizeof(ua));
  getsockname(u, (struct sockaddr *)&ua, &ulen);
  nonblock(u);
  poll_start(afd, &p, u, AFD_READ);
  check(wait_one(iocp, 100) == NULL, "a UDP read poll with no datagram waits");
  SOCKET u2 = socket(AF_INET, SOCK_DGRAM, 0);
  sendto(u2, "d", 1, 0, (struct sockaddr *)&ua, sizeof(ua));
  r = wait_one(iocp, 1000);
  check(r == &p && (p.info.Handles[0].Events & AFD_POLL_RECEIVE),
        "a datagram completes the UDP read poll with RECEIVE");

  printf("%d failures\n", failures);
  return failures != 0;
}
