/* The watchdog of the Linux world (design section 2.5, The limit of a busy
   domain). A C thread reads the beat of each domain: the scheduler adds one at
   the start of a fiber and one at its end, so an odd beat tells that a fiber
   runs. If the beat of a domain is odd and does not change for the limit, the
   thread writes a warning to stderr, one time for each stall. The thread
   blocks all signals, and it never runs OCaml code. */

#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "weft_posix.h"

struct watchdog {
  pthread_t thread;
  pthread_mutex_t m;
  pthread_cond_t c;
  int stop;
  volatile intnat *beats; /* The data of the Bigarray: OCaml keeps it alive. */
  int n;
  int64_t limit_ns;
};

/* The reports of all watchdogs, for the tests. */
static intnat reports = 0;

#define now_ns weft_now_ns

static void *watch(void *arg) {
  struct watchdog *w = arg;
  intnat *last = calloc(w->n, sizeof *last);
  int64_t *since = calloc(w->n, sizeof *since);
  int *told = calloc(w->n, sizeof *told);
  if (last == NULL || since == NULL || told == NULL) return NULL;
  int64_t t = now_ns();
  for (int d = 0; d < w->n; d++) {
    last[d] = w->beats[d];
    since[d] = t;
  }
  pthread_mutex_lock(&w->m);
  while (!w->stop) {
    /* A check each half of the limit. */
    weft_cond_wait_until(&w->c, &w->m, now_ns() + w->limit_ns / 2);
    if (w->stop) break;
    t = now_ns();
    for (int d = 0; d < w->n; d++) {
      intnat b = w->beats[d];
      if (b != last[d]) {
        last[d] = b;
        since[d] = t;
        told[d] = 0;
      } else if ((b & 1) && !told[d] && t - since[d] >= w->limit_ns) {
        told[d] = 1;
        __atomic_add_fetch(&reports, 1, __ATOMIC_RELAXED);
        fprintf(stderr,
                "weft: domain %d did not switch for %lld ms: a fiber computes "
                "with no yield\n",
                d, (long long)((t - since[d]) / 1000000));
      }
    }
  }
  pthread_mutex_unlock(&w->m);
  free(last);
  free(since);
  free(told);
  return NULL;
}

#define Watchdog_val(v) ((struct watchdog *)Nativeint_val(v))

/* [beats] is the Bigarray of the beats. The caller keeps it alive until the
   stop. */
value weft_watchdog_start(value beats, value limit_ns) {
  struct watchdog *w = calloc(1, sizeof *w);
  if (w == NULL) caml_raise_out_of_memory();
  w->beats = (volatile intnat *)Caml_ba_data_val(beats);
  w->n = Caml_ba_array_val(beats)->dim[0];
  w->limit_ns = Int64_val(limit_ns);
  pthread_mutex_init(&w->m, NULL);
  /* The timed wait uses the clock of the deadlines. */
  weft_cond_init(&w->c);
  sigset_t all, old;
  sigfillset(&all);
  pthread_sigmask(SIG_SETMASK, &all, &old);
  int e = pthread_create(&w->thread, NULL, watch, w);
  pthread_sigmask(SIG_SETMASK, &old, NULL);
  if (e != 0) {
    free(w);
    caml_failwith("weft: the watchdog thread did not start");
  }
  return caml_copy_nativeint((intnat)w);
}

value weft_watchdog_stop(value v) {
  struct watchdog *w = Watchdog_val(v);
  pthread_mutex_lock(&w->m);
  w->stop = 1;
  pthread_cond_signal(&w->c);
  pthread_mutex_unlock(&w->m);
  pthread_join(w->thread, NULL);
  pthread_mutex_destroy(&w->m);
  pthread_cond_destroy(&w->c);
  free(w);
  return Val_unit;
}

value weft_watchdog_reports(value unit) {
  return Val_long(__atomic_load_n(&reports, __ATOMIC_RELAXED));
}
