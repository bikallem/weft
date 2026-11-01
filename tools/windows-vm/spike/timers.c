/* The spike of the alarm and the wake for the Windows world (plan step 1).
   It measures how late a wait on an IOCP ends, in microseconds:

   - A: the timeout of GetQueuedCompletionStatusEx (milliseconds only).
   - B: the same after timeBeginPeriod (1).
   - C: a high-resolution waitable timer. NtAssociateWaitCompletionPacket
     connects the timer to the IOCP, thus the alarm is a completion, as the
     timerfd is an fd in the epoll set on Linux. Microsoft does not document
     this call in the SDK, but it is in ntdll from Windows 8, and the Go
     runtime uses it.
   - D: the exit of a child process through the same call (no thread waits).
   - E: a wake with PostQueuedCompletionStatus from another thread.

   Build in the VM: x86_64-w64-mingw32-gcc -O2 -o timers.exe timers.c
   -lwinmm */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winternl.h>
#include <timeapi.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef CREATE_WAITABLE_TIMER_HIGH_RESOLUTION
#define CREATE_WAITABLE_TIMER_HIGH_RESOLUTION 0x00000002
#endif

typedef NTSTATUS(NTAPI *create_packet_t)(PHANDLE, ACCESS_MASK,
                                         POBJECT_ATTRIBUTES);
typedef NTSTATUS(NTAPI *associate_t)(HANDLE, HANDLE, HANDLE, PVOID, PVOID,
                                     NTSTATUS, ULONG_PTR, PBOOLEAN);

static create_packet_t create_packet;
static associate_t associate;
static LARGE_INTEGER freq;

#define N 40

static double now_us(void) {
  LARGE_INTEGER t;
  QueryPerformanceCounter(&t);
  return (double)t.QuadPart * 1e6 / (double)freq.QuadPart;
}

static int cmp(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return (x > y) - (x < y);
}

/* Prints the lateness: the minimum, the median, the 95th percentile and the
   maximum. */
static void report(const char *what, double target_us, double *late) {
  qsort(late, N, sizeof(double), cmp);
  printf("%-44s target %7.0f us  late: min %6.0f  med %6.0f  p95 %6.0f  max "
         "%6.0f\n",
         what, target_us, late[0], late[N / 2], late[N * 95 / 100],
         late[N - 1]);
}

static void gqcs_timeout(HANDLE iocp, DWORD ms, const char *what) {
  double late[N];
  OVERLAPPED_ENTRY e;
  ULONG n;
  for (int i = 0; i < N; i++) {
    double t0 = now_us();
    GetQueuedCompletionStatusEx(iocp, &e, 1, &n, ms, FALSE);
    late[i] = now_us() - t0 - ms * 1000.0;
  }
  report(what, ms * 1000.0, late);
}

static void hires_timer(HANDLE iocp, double us) {
  HANDLE timer = CreateWaitableTimerExW(
      NULL, NULL, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION, TIMER_ALL_ACCESS);
  HANDLE packet;
  if (!timer || create_packet(&packet, GENERIC_ALL, NULL) != 0) {
    printf("FAIL  the high-resolution timer or the packet\n");
    exit(1);
  }
  double late[N];
  OVERLAPPED_ENTRY e;
  ULONG n;
  for (int i = 0; i < N; i++) {
    LARGE_INTEGER due;
    due.QuadPart = -(LONGLONG)(us * 10); /* Relative, in 100 ns. */
    double t0 = now_us();
    SetWaitableTimer(timer, &due, 0, NULL, NULL, FALSE);
    BOOLEAN signaled = FALSE;
    associate(packet, iocp, timer, (PVOID)1, NULL, 0, 0, &signaled);
    if (!GetQueuedCompletionStatusEx(iocp, &e, 1, &n, 5000, FALSE) ||
        e.lpCompletionKey != 1) {
      printf("FAIL  no completion of the timer\n");
      exit(1);
    }
    late[i] = now_us() - t0 - us;
  }
  char what[64];
  snprintf(what, sizeof(what), "C: high-resolution timer, %.0f us", us);
  report(what, us, late);
  CloseHandle(packet);
  CloseHandle(timer);
}

static void child_exit(HANDLE iocp) {
  STARTUPINFOW si = {sizeof(si)};
  PROCESS_INFORMATION pi;
  WCHAR cmd[] = L"cmd.exe /c exit 7";
  if (!CreateProcessW(NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL,
                      NULL, &si, &pi)) {
    printf("FAIL  CreateProcessW\n");
    return;
  }
  CloseHandle(pi.hThread);
  HANDLE packet;
  create_packet(&packet, GENERIC_ALL, NULL);
  BOOLEAN signaled = FALSE;
  associate(packet, iocp, pi.hProcess, (PVOID)2, NULL, 0, 0, &signaled);
  OVERLAPPED_ENTRY e;
  ULONG n;
  BOOL got = GetQueuedCompletionStatusEx(iocp, &e, 1, &n, 5000, FALSE);
  DWORD code = 0;
  GetExitCodeProcess(pi.hProcess, &code);
  printf("%s  D: the exit of a child is a completion (key %lu, exit code "
         "%lu)\n",
         got && e.lpCompletionKey == 2 && code == 7 ? "OK  " : "FAIL",
         got ? (unsigned long)e.lpCompletionKey : 0UL, (unsigned long)code);
  CloseHandle(packet);
  CloseHandle(pi.hProcess);
}

static HANDLE wake_iocp;
static volatile double wake_t0;

static DWORD WINAPI waker(LPVOID arg) {
  (void)arg;
  for (int i = 0; i < N; i++) {
    Sleep(2);
    wake_t0 = now_us();
    PostQueuedCompletionStatus(wake_iocp, 0, 3, NULL);
  }
  return 0;
}

static void wake_latency(void) {
  wake_iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
  HANDLE t = CreateThread(NULL, 0, waker, NULL, 0, NULL);
  double late[N];
  OVERLAPPED_ENTRY e;
  ULONG n;
  for (int i = 0; i < N; i++) {
    GetQueuedCompletionStatusEx(wake_iocp, &e, 1, &n, INFINITE, FALSE);
    late[i] = now_us() - wake_t0;
  }
  WaitForSingleObject(t, INFINITE);
  report("E: a wake from another thread", 0, late);
}

int main(void) {
  QueryPerformanceFrequency(&freq);
  HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
  create_packet =
      (create_packet_t)GetProcAddress(ntdll, "NtCreateWaitCompletionPacket");
  associate =
      (associate_t)GetProcAddress(ntdll, "NtAssociateWaitCompletionPacket");
  if (!create_packet || !associate) {
    printf("FAIL  ntdll has no wait completion packets\n");
    return 1;
  }
  HANDLE iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);

  gqcs_timeout(iocp, 1, "A: GQCSEx timeout, 1 ms");
  gqcs_timeout(iocp, 10, "A: GQCSEx timeout, 10 ms");
  hires_timer(iocp, 100);
  hires_timer(iocp, 1000);
  hires_timer(iocp, 10000);
  child_exit(iocp);
  wake_latency();
  timeBeginPeriod(1);
  gqcs_timeout(iocp, 1, "B: GQCSEx timeout, 1 ms, timeBeginPeriod 1");
  gqcs_timeout(iocp, 10, "B: GQCSEx timeout, 10 ms, timeBeginPeriod 1");
  timeEndPeriod(1);
  return 0;
}
