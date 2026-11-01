/* The parts of C that the Windows world shares between its files (design
   section 2.7, Windows). The stubs of weft.posix have a Windows version with
   the same contract as on POSIX: an fd is an int (a SOCKET or a HANDLE), and
   a stub gives a value >= 0, or -errno with the errno values of the MinGW-w64
   CRT. Thus Weft_posix.Make is the same for the three worlds. */

#ifndef WEFT_WIN_H
#define WEFT_WIN_H

#ifdef _WIN32

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0A00
#endif
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <errno.h>
#include <stdint.h>

/* The directions of an edge. */
#define WEFT_DIR_R 1
#define WEFT_DIR_W 2

/* The keys of the completions on the IOCP of a domain. */
#define WEFT_KEY_WAKE 1
#define WEFT_KEY_EDGE 2
#define WEFT_KEY_AFD 3
#define WEFT_KEY_PIPE 4
#define WEFT_KEY_PROC 5
#define WEFT_KEY_TIMER 6

/* An error of Win32 with no errno value: the base plus the code of Win32.
   weft_errname gives "WIN32_<code>" for it. */
#define WEFT_WIN32_BASE 100000

/* The errno value of an error of Win32 (GetLastError) or of Winsock
   (WSAGetLastError). */
int weft_win_errno(DWORD e);
int weft_wsa_errno(int e);

/* WSAStartup, one time. */
void weft_win_init(void);

/* UTF-8 to UTF-16, with malloc. NULL if out of memory or not valid. */
wchar_t *weft_wide(const char *s);

/* The table of the fds: a socket of the world. */
int weft_win_add_socket(SOCKET s, int dgram);

/* After WSAEWOULDBLOCK of an operation in direction [dir]: the next edge in
   that direction goes to the domains that registered the socket. */
void weft_win_arm(SOCKET s, int dir);

/* The error of a connect that failed, from the event of AFD, or 0. */
int weft_win_connect_error(SOCKET s);

/* After a reset of the connection: 0 the first time, then 1. Linux gives
   ECONNRESET one time, then the end of the stream to a read and EPIPE to a
   write. */
int weft_win_reset(SOCKET s);

/* 1 if the socket is a datagram socket of the table. */
int weft_win_is_dgram(SOCKET s);

/* The stubs of fds that the table knows: pipes, children and sockets. */
int weft_win_close(intptr_t fd);
int64_t weft_win_pipe_read(intptr_t fd, char *buf, int64_t len);
int64_t weft_win_pipe_write(intptr_t fd, const char *buf, int64_t len);
int weft_win_pipe(intptr_t *r, intptr_t *w);

#endif
#endif
