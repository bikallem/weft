/* The parts of C that the worlds share: Linux, macOS, FreeBSD and Windows. */

#ifndef WEFT_POSIX_H
#define WEFT_POSIX_H

#include <pthread.h>
#ifndef _WIN32
#include <signal.h>
#endif
#include <stdint.h>
#include <time.h>

#define ERR (-errno)

/* The BSD systems have a length field in each address, and FreeBSD refuses a
   bind without it. */
#if defined(__APPLE__) || defined(__FreeBSD__)
#define HAVE_SA_LEN 1
#endif

#ifdef _WIN32
#include "weft_win.h"

/* The clock of Weft_posix.now Mono (weft_win_stubs.c). */
static inline int64_t weft_now_ns(void) {
  LARGE_INTEGER f, c;
  QueryPerformanceFrequency(&f);
  QueryPerformanceCounter(&c);
  return (c.QuadPart / f.QuadPart) * 1000000000 + (c.QuadPart % f.QuadPart) * 1000000000 / f.QuadPart;
}
#else
static inline int64_t weft_now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}
#endif

/* A condition variable for waits until a time of CLOCK_MONOTONIC. macOS has
   no pthread_condattr_setclock, but it has a wait for a relative time. On
   Windows, the wait of winpthreads takes a time of CLOCK_REALTIME: the wait
   adds the time that is left to that clock. */
static inline void weft_cond_init(pthread_cond_t *c) {
#if defined(__APPLE__) || defined(_WIN32)
  pthread_cond_init(c, NULL);
#else
  pthread_condattr_t ca;
  pthread_condattr_init(&ca);
  pthread_condattr_setclock(&ca, CLOCK_MONOTONIC);
  pthread_cond_init(c, &ca);
  pthread_condattr_destroy(&ca);
#endif
}

static inline void weft_cond_wait_until(pthread_cond_t *c, pthread_mutex_t *m, int64_t at) {
#ifdef __APPLE__
  int64_t d = at - weft_now_ns();
  if (d < 0) d = 0;
  struct timespec ts = {d / 1000000000, d % 1000000000};
  pthread_cond_timedwait_relative_np(c, m, &ts);
#elif defined(_WIN32)
  int64_t d = at - weft_now_ns();
  if (d < 0) d = 0;
  struct timespec now;
  clock_gettime(CLOCK_REALTIME, &now);
  int64_t t = (int64_t)now.tv_sec * 1000000000 + now.tv_nsec + d;
  struct timespec ts = {t / 1000000000, t % 1000000000};
  pthread_cond_timedwait(c, m, &ts);
#else
  struct timespec ts = {at / 1000000000, at % 1000000000};
  pthread_cond_timedwait(c, m, &ts);
#endif
}

/* A new thread copies the signal mask of its creator. Thus the creator
   blocks all signals for the time of the create, and a thread of the
   library never gets a signal of the user. Windows has no signals. */
static inline int weft_thread_create(pthread_t *t, const pthread_attr_t *attr,
                                     void *(*f)(void *), void *arg) {
#ifdef _WIN32
  return pthread_create(t, attr, f, arg);
#else
  sigset_t all, old;
  sigfillset(&all);
  pthread_sigmask(SIG_SETMASK, &all, &old);
  int e = pthread_create(t, attr, f, arg);
  pthread_sigmask(SIG_SETMASK, &old, NULL);
  return e;
#endif
}

#endif
