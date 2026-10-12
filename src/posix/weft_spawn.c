/* The spawn of a child for the POSIX worlds (design section 2.7). Linux uses
   pidfd_spawnp, so the pidfd exists before any code can reap the child. The
   BSD systems use posix_spawnp, and the world makes the fd of the exit.

   No fd of the library goes into a child, also if another domain makes an fd
   at the same time. Linux and FreeBSD make each fd with its close-on-exec
   flag. macOS has no pipe2, so its pipes get the flag after the create, and
   each spawn uses POSIX_SPAWN_CLOEXEC_DEFAULT: the child gets only the fds 0,
   1 and 2. */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <stdlib.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/pidfd.h>
#endif

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

extern char **environ;

/* A NULL-terminated copy of an array of strings. The strings stay in the OCaml
   heap: the call does not allocate in it, so the GC cannot move them. */
static char **c_strings(value a) {
  mlsize_t n = Wosize_val(a);
  char **c = malloc((n + 1) * sizeof *c);
  if (c == NULL) return NULL;
  for (mlsize_t i = 0; i < n; i++) c[i] = (char *)String_val(Field(a, i));
  c[n] = NULL;
  return c;
}

static int cloexec_pipe(int p[2]) {
#ifdef __APPLE__
  if (pipe(p) < 0) return -1;
  fcntl(p[0], F_SETFD, FD_CLOEXEC);
  fcntl(p[1], F_SETFD, FD_CLOEXEC);
  return 0;
#else
  return pipe2(p, O_CLOEXEC);
#endif
}

/* [weft_spawn args defaults]. [args] is (prog, argv, env, has_env, cwd,
   stdio): stdio has a kind for the fds 0, 1 and 2 (0 inherit, 1 /dev/null, 2
   a pipe). [defaults] are the signals that get the default action in the
   child: the signals that the world ignores. The child gets no blocked
   signals. The result is (pid, pidfd, stdin, stdout, stderr): the parent ends
   of the pipes, or -1. pidfd is -1 on the BSD systems. pid is -errno on an
   error. */
value weft_spawn(value args, value defaults) {
  CAMLparam2(args, defaults);
  CAMLlocal1(r);
  value prog = Field(args, 0), stdio = Field(args, 5);
  int ends[3] = {-1, -1, -1}, child[3] = {-1, -1, -1};
  int pidfd = -1, e = 0;
  pid_t pid = 0;
  posix_spawn_file_actions_t fa;
  posix_spawnattr_t at;
  sigset_t none, def;
  short flags = POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF;
  char **argv = c_strings(Field(args, 1));
  char **envp = Bool_val(Field(args, 3)) ? c_strings(Field(args, 2)) : environ;
  if (argv == NULL || envp == NULL) {
    free(argv);
    if (envp != environ) free(envp);
    caml_raise_out_of_memory();
  }
  posix_spawn_file_actions_init(&fa);
  posix_spawnattr_init(&at);
  for (int i = 0; i < 3 && e == 0; i++) {
    int kind = Int_val(Field(stdio, i));
    if (kind == 0) {
#ifdef __APPLE__
      e = posix_spawn_file_actions_addinherit_np(&fa, i);
#endif
    } else if (kind == 1) {
      e = posix_spawn_file_actions_addopen(&fa, i, "/dev/null", O_RDWR, 0);
    } else {
      int p[2];
      if (cloexec_pipe(p) < 0) {
        e = errno;
        break;
      }
      /* The child gets fd 0 as the read end, and the fds 1 and 2 as write
         ends. Only the end of the parent is non-blocking. */
      ends[i] = i == 0 ? p[1] : p[0];
      child[i] = i == 0 ? p[0] : p[1];
      fcntl(ends[i], F_SETFL, O_NONBLOCK);
      e = posix_spawn_file_actions_adddup2(&fa, child[i], i);
    }
  }
  if (e == 0 && caml_string_length(Field(args, 4)) > 0)
    e = posix_spawn_file_actions_addchdir_np(&fa, String_val(Field(args, 4)));
  if (e == 0) {
    sigemptyset(&none);
    sigemptyset(&def);
    for (mlsize_t i = 0; i < Wosize_val(defaults); i++)
      sigaddset(&def, Int_val(Field(defaults, i)));
    posix_spawnattr_setsigmask(&at, &none);
    posix_spawnattr_setsigdefault(&at, &def);
#ifdef __APPLE__
    flags |= POSIX_SPAWN_CLOEXEC_DEFAULT;
#endif
    posix_spawnattr_setflags(&at, flags);
#ifdef __linux__
    e = pidfd_spawnp(&pidfd, String_val(prog), &fa, &at, argv, envp);
    if (e == 0) {
      pid = pidfd_getpid(pidfd);
      if (pid < 0) pid = 0;
    }
#else
    e = posix_spawnp(&pid, String_val(prog), &fa, &at, argv, envp);
#endif
  }
  posix_spawn_file_actions_destroy(&fa);
  posix_spawnattr_destroy(&at);
  free(argv);
  if (envp != environ) free(envp);
  for (int i = 0; i < 3; i++) {
    if (child[i] >= 0) close(child[i]);
    if (e != 0 && ends[i] >= 0) {
      close(ends[i]);
      ends[i] = -1;
    }
  }
  r = caml_alloc_tuple(5);
  Store_field(r, 0, Val_int(e != 0 ? -e : pid));
  Store_field(r, 1, Val_int(pidfd));
  for (int i = 0; i < 3; i++) Store_field(r, 2 + i, Val_int(ends[i]));
  CAMLreturn(r);
}
