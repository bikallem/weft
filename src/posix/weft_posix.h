/* The parts of C that the POSIX worlds share: Linux, macOS and FreeBSD. */

#ifndef WEFT_POSIX_H
#define WEFT_POSIX_H

#include <pthread.h>
#include <stdint.h>
#include <time.h>

#define ERR (-errno)

/* The BSD systems have a length field in each address, and FreeBSD refuses a
   bind without it. */
#if defined(__APPLE__) || defined(__FreeBSD__)
#define HAVE_SA_LEN 1
#endif

static inline int64_t weft_now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}

/* A condition variable for waits until a time of CLOCK_MONOTONIC. macOS has
   no pthread_condattr_setclock, but it has a wait for a relative time. */
static inline void weft_cond_init(pthread_cond_t *c) {
#ifdef __APPLE__
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
#else
  struct timespec ts = {at / 1000000000, at % 1000000000};
  pthread_cond_timedwait(c, m, &ts);
#endif
}

#endif
