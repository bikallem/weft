/* The thread pools of the Linux world (design section 2.3). The threads of a
   pool never run OCaml code, and they block all signals. A job never uses the
   memory of the user: a read fills a buffer of the job, and a write copies the
   data at the start. A completed job goes to the mailbox of the domain that
   started it, and a write to the eventfd of that domain wakes it.

   There are two pools: one for the names (getaddrinfo, getnameinfo), and one
   for the files and the directories. Thus a DNS server that does not answer
   cannot stop the file operations.

   The life of a job: the start makes it, and a worker runs it. Then the job
   is done. [weft_job_free] (after the result) or a detach frees a job that is
   done. A detach before the end marks the job, and the worker frees it. The
   pool mutex protects [done] and [detached]. */

#define _GNU_SOURCE
#include <arpa/inet.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

enum {
  J_OPEN,
  J_PREAD,
  J_PWRITE,
  J_FSYNC,
  J_FSTAT,
  J_STAT,
  J_FTRUNCATE,
  J_MKDIR,
  J_READDIR,
  J_RENAME,
  J_UNLINK,
  J_RMDIR,
  J_RESOLVE,
  J_REVERSE,
  J_CLOSE
};

/* The mailbox of one domain: the ids of the completed jobs. */
struct box {
  pthread_mutex_t m;
  int64_t *ids;
  int n, cap;
  int evfd;
  int closed; /* The world closed: the eventfd can be closed. */
};

/* The mailboxes of one world. Each job that can post holds a reference. */
struct mbx {
  int refs;
  int ndoms;
  struct box boxes[];
};

struct job {
  struct job *next;
  int kind;
  int64_t id;
  int dom;
  struct mbx *mbx; /* NULL: nobody gets the completion. */
  struct pool *pool;
  char *s1, *s2;
  int fd;
  int64_t a, b;
  char *buf; /* The data of a write, or the result. */
  size_t len;
  int64_t res;
  int err; /* 0, an errno value, or -1 for a getaddrinfo error in buf. */
  struct stat st;
  int done, detached;
};

struct pool {
  pthread_mutex_t m;
  pthread_cond_t c;
  struct job *head, *tail;
  int queued, threads, idle, max;
};

static struct pool file_pool = {PTHREAD_MUTEX_INITIALIZER,
                                PTHREAD_COND_INITIALIZER, NULL, NULL, 0, 0, 0, 16};
static struct pool dns_pool = {PTHREAD_MUTEX_INITIALIZER,
                               PTHREAD_COND_INITIALIZER, NULL, NULL, 0, 0, 0, 8};

static void unref(struct mbx *w) {
  if (__atomic_sub_fetch(&w->refs, 1, __ATOMIC_ACQ_REL) == 0) {
    for (int i = 0; i < w->ndoms; i++) {
      pthread_mutex_destroy(&w->boxes[i].m);
      free(w->boxes[i].ids);
    }
    free(w);
  }
}

static void post(struct mbx *w, int dom, int64_t id) {
  struct box *b = &w->boxes[dom];
  pthread_mutex_lock(&b->m);
  if (!b->closed) {
    if (b->n == b->cap) {
      int cap = b->cap ? 2 * b->cap : 16;
      int64_t *ids = realloc(b->ids, cap * sizeof *ids);
      if (ids == NULL) abort();
      b->ids = ids;
      b->cap = cap;
    }
    b->ids[b->n++] = id;
    /* Under the lock: the close of the world cannot close the fd here. */
    uint64_t one = 1;
    ssize_t r = write(b->evfd, &one, sizeof one);
    (void)r;
  }
  pthread_mutex_unlock(&b->m);
}

static void free_job(struct job *j) {
  free(j->s1);
  free(j->s2);
  free(j->buf);
  free(j);
}

/* The cleanup of a result that nobody gets. */
static void drop(struct job *j) {
  if (j->kind == J_OPEN && j->res >= 0) close((int)j->res);
}

/* Add [n] bytes to the result buffer. */
static int append(struct job *j, size_t *cap, const char *s, size_t n) {
  if (j->len + n > *cap) {
    size_t c = *cap ? *cap : 256;
    while (c < j->len + n) c *= 2;
    char *b = realloc(j->buf, c);
    if (b == NULL) return -1;
    j->buf = b;
    *cap = c;
  }
  memcpy(j->buf + j->len, s, n);
  j->len += n;
  return 0;
}

static int open_flags(int64_t f) {
  int rd = f & 1, wr = f & 2;
  int fl = O_CLOEXEC | (wr ? (rd ? O_RDWR : O_WRONLY) : O_RDONLY);
  if (f & 4) fl |= O_CREAT;
  if (f & 8) fl |= O_EXCL;
  if (f & 16) fl |= O_TRUNC;
  if (f & 32) fl |= O_APPEND;
  return fl;
}

static void name_error(struct job *j, int e) {
  if (e == EAI_NONAME
#ifdef EAI_NODATA
      || e == EAI_NODATA
#endif
  ) {
    j->err = ENOENT;
  } else if (e == EAI_SYSTEM) {
    j->err = errno;
  } else {
    size_t cap = 0;
    const char *m = gai_strerror(e);
    j->len = 0;
    if (append(j, &cap, m, strlen(m)) == 0) j->err = -1;
    else j->err = ENOMEM;
  }
  j->res = -1;
}

static void resolve(struct job *j) {
  struct addrinfo hints, *ai, *p;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  int e = getaddrinfo(j->s1, NULL, &hints, &ai);
  if (e != 0) {
    name_error(j, e);
    return;
  }
  size_t cap = 0;
  char text[INET6_ADDRSTRLEN];
  for (p = ai; p != NULL; p = p->ai_next) {
    const void *a = p->ai_family == AF_INET
                        ? (const void *)&((struct sockaddr_in *)p->ai_addr)->sin_addr
                        : (const void *)&((struct sockaddr_in6 *)p->ai_addr)->sin6_addr;
    if ((p->ai_family == AF_INET || p->ai_family == AF_INET6) &&
        inet_ntop(p->ai_family, a, text, sizeof text) != NULL)
      append(j, &cap, text, strlen(text) + 1);
  }
  freeaddrinfo(ai);
  j->res = 0;
}

static void reverse(struct job *j) {
  struct sockaddr_storage ss;
  socklen_t sl;
  memset(&ss, 0, sizeof ss);
  struct sockaddr_in *v4 = (struct sockaddr_in *)&ss;
  struct sockaddr_in6 *v6 = (struct sockaddr_in6 *)&ss;
  if (inet_pton(AF_INET, j->s1, &v4->sin_addr) == 1) {
    v4->sin_family = AF_INET;
    sl = sizeof *v4;
  } else if (inet_pton(AF_INET6, j->s1, &v6->sin6_addr) == 1) {
    v6->sin6_family = AF_INET6;
    sl = sizeof *v6;
  } else {
    j->err = EINVAL;
    j->res = -1;
    return;
  }
  char host[NI_MAXHOST];
  int e = getnameinfo((struct sockaddr *)&ss, sl, host, sizeof host, NULL, 0,
                      NI_NAMEREQD);
  if (e != 0) {
    name_error(j, e);
    return;
  }
  size_t cap = 0;
  append(j, &cap, host, strlen(host));
  j->res = 0;
}

static void read_dir(struct job *j) {
  DIR *d = opendir(j->s1);
  if (d == NULL) {
    j->res = -1;
    j->err = errno;
    return;
  }
  size_t cap = 0;
  struct dirent *e;
  errno = 0;
  while ((e = readdir(d)) != NULL) {
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    if (append(j, &cap, e->d_name, strlen(e->d_name) + 1) < 0) {
      errno = ENOMEM;
      break;
    }
  }
  j->err = errno;
  j->res = errno ? -1 : 0;
  closedir(d);
}

static void run(struct job *j) {
  int64_t r = 0;
  switch (j->kind) {
  case J_OPEN:
    r = open(j->s1, open_flags(j->a), (mode_t)j->b);
    break;
  case J_PREAD:
    j->buf = malloc(j->a > 0 ? j->a : 1);
    if (j->buf == NULL) {
      r = -1;
      errno = ENOMEM;
      break;
    }
    r = j->b < 0 ? read(j->fd, j->buf, j->a) : pread(j->fd, j->buf, j->a, j->b);
    if (r >= 0) j->len = r;
    break;
  case J_PWRITE:
    r = j->b < 0 ? write(j->fd, j->buf, j->len) : pwrite(j->fd, j->buf, j->len, j->b);
    break;
  case J_FSYNC:
    r = fsync(j->fd);
    break;
  case J_FSTAT:
    r = fstat(j->fd, &j->st);
    break;
  case J_STAT:
    r = stat(j->s1, &j->st);
    break;
  case J_FTRUNCATE:
    r = ftruncate(j->fd, j->a);
    break;
  case J_MKDIR:
    r = mkdir(j->s1, (mode_t)j->b);
    break;
  case J_READDIR:
    read_dir(j);
    return;
  case J_RENAME:
    r = rename(j->s1, j->s2);
    break;
  case J_UNLINK:
    r = unlink(j->s1);
    break;
  case J_RMDIR:
    r = rmdir(j->s1);
    break;
  case J_RESOLVE:
    resolve(j);
    return;
  case J_REVERSE:
    reverse(j);
    return;
  case J_CLOSE:
    r = close(j->fd);
    break;
  }
  j->res = r;
  j->err = r < 0 ? errno : 0;
}

static void *worker(void *arg) {
  struct pool *p = arg;
  pthread_mutex_lock(&p->m);
  for (;;) {
    while (p->head == NULL) {
      p->idle++;
      pthread_cond_wait(&p->c, &p->m);
      p->idle--;
    }
    struct job *j = p->head;
    p->head = j->next;
    if (p->head == NULL) p->tail = NULL;
    p->queued--;
    pthread_mutex_unlock(&p->m);
    run(j);
    pthread_mutex_lock(&p->m);
    j->done = 1;
    /* After the unlock, a detach can free the job. */
    struct mbx *w = j->mbx;
    int dom = j->dom;
    int64_t id = j->id;
    int detached = j->detached;
    if (detached) drop(j);
    pthread_mutex_unlock(&p->m);
    if (w != NULL) {
      post(w, dom, id);
      unref(w);
    }
    if (detached) free_job(j);
    pthread_mutex_lock(&p->m);
  }
  return NULL;
}

/* A new thread copies the signal mask of its creator. Thus the creator blocks
   all signals for the time of the create. */
static int spawn(struct pool *p) {
  sigset_t all, old;
  pthread_attr_t attr;
  pthread_t t;
  sigfillset(&all);
  pthread_sigmask(SIG_SETMASK, &all, &old);
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  int ok = pthread_create(&t, &attr, worker, p) == 0;
  if (ok) p->threads++;
  pthread_attr_destroy(&attr);
  pthread_sigmask(SIG_SETMASK, &old, NULL);
  return ok;
}

/* With no worker, the queued jobs complete with EAGAIN. Under the pool
   mutex. */
static void fail_all(struct pool *p) {
  struct job *j = p->head;
  p->head = p->tail = NULL;
  p->queued = 0;
  while (j != NULL) {
    struct job *next = j->next;
    j->res = -1;
    j->err = EAGAIN;
    j->done = 1;
    struct mbx *w = j->mbx;
    if (w != NULL) {
      post(w, j->dom, j->id);
      unref(w);
    }
    if (j->detached) free_job(j);
    j = next;
  }
}

/* After a fork, the child has no workers: it starts new ones, and it drops
   the jobs of the parent. The pool mutexes are free in the child, because the
   fork takes them first. */
static void prepare(void) {
  pthread_mutex_lock(&file_pool.m);
  pthread_mutex_lock(&dns_pool.m);
}

static void parent(void) {
  pthread_mutex_unlock(&dns_pool.m);
  pthread_mutex_unlock(&file_pool.m);
}

static void reset(struct pool *p) {
  pthread_mutex_init(&p->m, NULL);
  pthread_cond_init(&p->c, NULL);
  p->head = p->tail = NULL;
  p->queued = p->threads = p->idle = 0;
}

static void child(void) {
  reset(&file_pool);
  reset(&dns_pool);
}

static pthread_once_t once = PTHREAD_ONCE_INIT;

static void init(void) { pthread_atfork(prepare, parent, child); }

static void submit(struct job *j) {
  struct pool *p = j->pool;
  pthread_once(&once, init);
  pthread_mutex_lock(&p->m);
  j->next = NULL;
  if (p->tail) p->tail->next = j;
  else p->head = j;
  p->tail = j;
  p->queued++;
  /* A new thread for each job that no idle thread can take: a job that blocks
     does not stop the jobs after it, up to the maximum. */
  if (p->queued > p->idle && p->threads < p->max && !spawn(p) && p->threads == 0)
    fail_all(p);
  else pthread_cond_signal(&p->c);
  pthread_mutex_unlock(&p->m);
}

static char *copy_string(value s) {
  size_t n = caml_string_length(s);
  char *c = malloc(n + 1);
  if (c != NULL) {
    memcpy(c, String_val(s), n);
    c[n] = 0;
  }
  return c;
}

#define Mbx_val(v) ((struct mbx *)Nativeint_val(v))
#define Job_val(v) ((struct job *)Nativeint_val(v))

value weft_mbx_create(value evfds) {
  int n = Wosize_val(evfds);
  struct mbx *w = calloc(1, sizeof *w + n * sizeof(struct box));
  if (w == NULL) caml_raise_out_of_memory();
  w->refs = 1;
  w->ndoms = n;
  for (int i = 0; i < n; i++) {
    pthread_mutex_init(&w->boxes[i].m, NULL);
    w->boxes[i].evfd = Int_val(Field(evfds, i));
  }
  return caml_copy_nativeint((intnat)w);
}

/* After this call, no worker writes an eventfd of the world. */
value weft_mbx_close(value mbx) {
  struct mbx *w = Mbx_val(mbx);
  for (int i = 0; i < w->ndoms; i++) {
    pthread_mutex_lock(&w->boxes[i].m);
    w->boxes[i].closed = 1;
    pthread_mutex_unlock(&w->boxes[i].m);
  }
  unref(w);
  return Val_unit;
}

/* The ids of the completed jobs of a domain. */
value weft_mbx_take(value mbx, value dom) {
  CAMLparam2(mbx, dom);
  CAMLlocal1(r);
  struct box *b = &Mbx_val(mbx)->boxes[Int_val(dom)];
  pthread_mutex_lock(&b->m);
  int n = b->n;
  int64_t small[64];
  int64_t *ids = n <= 64 ? small : malloc(n * sizeof *ids);
  if (ids == NULL) {
    pthread_mutex_unlock(&b->m);
    caml_raise_out_of_memory();
  }
  memcpy(ids, b->ids, n * sizeof *ids);
  b->n = 0;
  pthread_mutex_unlock(&b->m);
  r = caml_alloc_tuple(n);
  for (int i = 0; i < n; i++) Store_field(r, i, Val_long(ids[i]));
  if (ids != small) free(ids);
  CAMLreturn(r);
}

/* [args] is the record [Weft_linux.job_args]: id, domain, kind, s1, s2, fd, a,
   b and data. It gives the job, or raises [Out_of_memory]. */
value weft_job_start(value mbx, value args) {
  struct job *j = calloc(1, sizeof *j);
  if (j == NULL) caml_raise_out_of_memory();
  j->id = Long_val(Field(args, 0));
  j->dom = Int_val(Field(args, 1));
  j->kind = Int_val(Field(args, 2));
  j->s1 = copy_string(Field(args, 3));
  j->s2 = copy_string(Field(args, 4));
  j->fd = Int_val(Field(args, 5));
  j->a = Long_val(Field(args, 6));
  j->b = Long_val(Field(args, 7));
  if (j->kind == J_PWRITE) {
    j->len = caml_string_length(Field(args, 8));
    j->buf = malloc(j->len ? j->len : 1);
    if (j->buf) memcpy(j->buf, String_val(Field(args, 8)), j->len);
  }
  if (j->s1 == NULL || j->s2 == NULL || (j->kind == J_PWRITE && j->buf == NULL)) {
    free_job(j);
    caml_raise_out_of_memory();
  }
  j->pool = (j->kind == J_RESOLVE || j->kind == J_REVERSE) ? &dns_pool : &file_pool;
  j->mbx = Mbx_val(mbx);
  __atomic_add_fetch(&j->mbx->refs, 1, __ATOMIC_ACQ_REL);
  submit(j);
  return caml_copy_nativeint((intnat)j);
}

/* The close(2) of a file on the pool, with no completion. */
value weft_close_async(value fd) {
  struct job *j = calloc(1, sizeof *j);
  if (j == NULL) caml_raise_out_of_memory();
  j->kind = J_CLOSE;
  j->fd = Int_val(fd);
  j->detached = 1;
  j->pool = &file_pool;
  submit(j);
  return Val_unit;
}

/* The result of a job that is done: (res, err, data). */
value weft_job_result(value job) {
  CAMLparam1(job);
  CAMLlocal2(r, data);
  struct job *j = Job_val(job);
  data = caml_alloc_initialized_string(j->len, j->buf ? j->buf : "");
  r = caml_alloc_tuple(3);
  Store_field(r, 0, Val_long(j->res));
  Store_field(r, 1, Val_int(j->err));
  Store_field(r, 2, data);
  CAMLreturn(r);
}

/* The stat of a job that is done: (kind, size, perm, mtime in ns). The kinds
   are 0 regular, 1 directory, 2 symlink and 3 other. */
value weft_job_stat(value job) {
  CAMLparam1(job);
  CAMLlocal1(r);
  struct stat *s = &Job_val(job)->st;
  int kind = S_ISREG(s->st_mode) ? 0 : S_ISDIR(s->st_mode) ? 1 : S_ISLNK(s->st_mode) ? 2 : 3;
  r = caml_alloc_tuple(4);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, Val_long(s->st_size));
  Store_field(r, 2, Val_int(s->st_mode & 07777));
  Store_field(r, 3,
              caml_copy_int64((int64_t)s->st_mtim.tv_sec * 1000000000 + s->st_mtim.tv_nsec));
  CAMLreturn(r);
}

value weft_job_free(value job) {
  free_job(Job_val(job));
  return Val_unit;
}

/* A job that nobody waits for. If it is done, drop its result now. If not,
   the worker drops it. */
value weft_job_detach(value job) {
  struct job *j = Job_val(job);
  struct pool *p = j->pool;
  pthread_mutex_lock(&p->m);
  int done = j->done;
  if (!done) j->detached = 1;
  pthread_mutex_unlock(&p->m);
  if (done) {
    drop(j);
    free_job(j);
  }
  return Val_unit;
}
