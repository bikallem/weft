/* The file jobs of the thread pools on Windows (weft_pool.c includes this
   file). A path is UTF-8, and Win32 takes "/" and "\" as separators. The
   checks give the errors of POSIX where Windows has other errors
   (portable.ml, the files tests). */

/* The offset of a file: a pread does not move it on POSIX, but a ReadFile at
   an offset moves it. Thus a read at an offset keeps the offset under a lock
   of the handle, and a read at the offset takes the same lock. Only a disk
   file takes the lock: a read of a pipe can block, and an offset has no
   use there. */
static SRWLOCK offset_locks[64];

static SRWLOCK *offset_lock(HANDLE h) { return &offset_locks[((uintptr_t)h >> 2) % 64]; }

static DWORD attrs_of(const wchar_t *p) { return GetFileAttributesW(p); }

static int is_dir_attr(DWORD a) {
  return a != INVALID_FILE_ATTRIBUTES && (a & FILE_ATTRIBUTE_DIRECTORY);
}

static int is_sep(wchar_t c) { return c == L'/' || c == L'\\'; }

/* ENOENT where a parent of the path is not a directory: POSIX gives ENOTDIR.
   A separator at the end also makes the name a directory. */
static int fix_enoent(const wchar_t *p, int e) {
  if (e != ENOENT) return e;
  wchar_t *c = _wcsdup(p);
  if (c == NULL) return e;
  size_t n = wcslen(c);
  for (size_t i = 1; i < n; i++) {
    if (!is_sep(c[i]) || is_sep(c[i - 1]) || c[i - 1] == L':') continue;
    wchar_t s = c[i];
    c[i] = 0;
    DWORD a = attrs_of(c);
    c[i] = s;
    if (a != INVALID_FILE_ATTRIBUTES && !(a & FILE_ATTRIBUTE_DIRECTORY)) {
      free(c);
      return ENOTDIR;
    }
  }
  free(c);
  return e;
}

/* A name with a separator at the end that is not a directory: ENOTDIR, or
   0. */
static int trailing(const wchar_t *p) {
  size_t n = wcslen(p);
  if (n < 2 || !is_sep(p[n - 1])) return 0;
  return fix_enoent(p, ENOENT) == ENOTDIR ? ENOTDIR : 0;
}

static int last_err(const wchar_t *p) { return fix_enoent(p, weft_win_errno(GetLastError())); }

/* The stat of a handle. Each open of this file has no
   FILE_FLAG_OPEN_REPARSE_POINT, thus the open follows a symbolic link and a
   junction, as stat(2) and fstat(2) do on POSIX. A reparse point that stays on
   the handle is not a link: for example a OneDrive placeholder or a
   deduplicated file. Thus the kind is Directory or Regular, never Symlink
   (issue 51). */
static void stat_of(struct job *j, BY_HANDLE_FILE_INFORMATION *fi) {
  DWORD a = fi->dwFileAttributes;
  j->st.kind = (a & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
  j->st.size = ((int64_t)fi->nFileSizeHigh << 32) | fi->nFileSizeLow;
  j->st.perm = (a & FILE_ATTRIBUTE_DIRECTORY) ? 0755 : (a & FILE_ATTRIBUTE_READONLY) ? 0444 : 0644;
  j->st.mtime = weft_filetime_ns(fi->ftLastWriteTime);
}

static void fail(struct job *j, int e) {
  j->res = -1;
  j->err = e;
}

static int64_t w_open(struct job *j, const wchar_t *p) {
  int f = (int)j->a, rd = f & 1, wr = f & 2, cr = f & 4, ex = f & 8, tr = f & 16, ap = f & 32;
  int t = trailing(p);
  if (t) return -t;
  DWORD access = 0;
  if (rd || !wr) access |= GENERIC_READ;
  if (wr) access |= ap && !tr ? FILE_APPEND_DATA | FILE_WRITE_ATTRIBUTES | SYNCHRONIZE : GENERIC_WRITE;
  DWORD disp = cr && ex ? CREATE_NEW : cr && tr ? CREATE_ALWAYS : cr ? OPEN_ALWAYS
                                     : tr       ? TRUNCATE_EXISTING
                                                : OPEN_EXISTING;
  HANDLE h = CreateFileW(p, access, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                         disp, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    DWORD e = GetLastError();
    if (e == ERROR_ACCESS_DENIED && is_dir_attr(attrs_of(p))) return -EISDIR;
    return -fix_enoent(p, weft_win_errno(e));
  }
  /* A directory opens for a read, as on POSIX, but not for a write. */
  if (wr) {
    BY_HANDLE_FILE_INFORMATION fi;
    if (GetFileInformationByHandle(h, &fi) && (fi.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
      CloseHandle(h);
      return -EISDIR;
    }
  }
  return (int64_t)(intptr_t)h;
}

/* A read or a write at [at], or at the offset of the file if [at] < 0. The
   errors: a handle with no access in this direction gives EBADF, and a
   directory gives EISDIR, as on POSIX. */
static int64_t w_io(HANDLE h, char *buf, int64_t len, int64_t at, int write) {
  DWORD n = 0, k = len > (1 << 30) ? (1 << 30) : (DWORD)len;
  BOOL ok;
  SRWLOCK *l = GetFileType(h) == FILE_TYPE_DISK ? offset_lock(h) : NULL;
  if (l != NULL) AcquireSRWLockExclusive(l);
  if (at < 0) {
    ok = write ? WriteFile(h, buf, k, &n, NULL) : ReadFile(h, buf, k, &n, NULL);
  } else {
    LARGE_INTEGER pos, zero = {0};
    SetFilePointerEx(h, zero, &pos, FILE_CURRENT);
    OVERLAPPED ov;
    memset(&ov, 0, sizeof ov);
    ov.Offset = (DWORD)at;
    ov.OffsetHigh = (DWORD)(at >> 32);
    ok = write ? WriteFile(h, buf, k, &n, &ov) : ReadFile(h, buf, k, &n, &ov);
    SetFilePointerEx(h, pos, NULL, FILE_BEGIN);
  }
  DWORD e = ok ? 0 : GetLastError();
  if (l != NULL) ReleaseSRWLockExclusive(l);
  if (ok) return n;
  if (!write && (e == ERROR_HANDLE_EOF || e == ERROR_BROKEN_PIPE)) return 0;
  if (e == ERROR_ACCESS_DENIED || e == ERROR_INVALID_HANDLE) return -EBADF;
  if (e == ERROR_INVALID_FUNCTION) return -EISDIR;
  return -weft_win_errno(e);
}

static int64_t w_stat_path(struct job *j, const wchar_t *p) {
  int t = trailing(p);
  if (t) return -t;
  HANDLE h = CreateFileW(p, FILE_READ_ATTRIBUTES,
                         FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                         OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (h == INVALID_HANDLE_VALUE) return -last_err(p);
  BY_HANDLE_FILE_INFORMATION fi;
  int64_t r = 0;
  if (GetFileInformationByHandle(h, &fi)) stat_of(j, &fi);
  else r = -weft_win_errno(GetLastError());
  CloseHandle(h);
  return r;
}

static void w_readdir(struct job *j, const wchar_t *p) {
  int t = trailing(p);
  if (t) {
    fail(j, t);
    return;
  }
  size_t n = wcslen(p);
  wchar_t *pat = malloc((n + 3) * sizeof *pat);
  if (pat == NULL) {
    fail(j, ENOMEM);
    return;
  }
  wcscpy(pat, p);
  if (n > 0 && !is_sep(p[n - 1])) wcscat(pat, L"\\");
  wcscat(pat, L"*");
  WIN32_FIND_DATAW fd;
  HANDLE h = FindFirstFileExW(pat, FindExInfoBasic, &fd, FindExSearchNameMatch, NULL, 0);
  free(pat);
  if (h == INVALID_HANDLE_VALUE) {
    DWORD e = GetLastError();
    DWORD a = attrs_of(p);
    if (a != INVALID_FILE_ATTRIBUTES && !(a & FILE_ATTRIBUTE_DIRECTORY)) fail(j, ENOTDIR);
    else if (e == ERROR_FILE_NOT_FOUND && is_dir_attr(a)) j->res = 0;
    else fail(j, fix_enoent(p, weft_win_errno(e)));
    return;
  }
  size_t cap = 0;
  char name[MAX_PATH * 4];
  do {
    if (wcscmp(fd.cFileName, L".") == 0 || wcscmp(fd.cFileName, L"..") == 0) continue;
    int k = WideCharToMultiByte(CP_UTF8, 0, fd.cFileName, -1, name, sizeof name, NULL, NULL);
    if (k > 0 && append(j, &cap, name, (size_t)k) < 0) {
      FindClose(h);
      fail(j, ENOMEM);
      return;
    }
  } while (FindNextFileW(h, &fd));
  FindClose(h);
  j->res = 0;
}

/* A rename replaces a file, or an empty directory with a directory, as on
   POSIX. Windows replaces only a file. */
static int64_t w_rename(const wchar_t *src, const wchar_t *dst) {
  DWORD as = attrs_of(src);
  if (as == INVALID_FILE_ATTRIBUTES) return -last_err(src);
  DWORD ad = attrs_of(dst);
  if (ad != INVALID_FILE_ATTRIBUTES) {
    int sdir = (as & FILE_ATTRIBUTE_DIRECTORY) != 0, ddir = (ad & FILE_ATTRIBUTE_DIRECTORY) != 0;
    if (!sdir && ddir) return -EISDIR;
    if (sdir && !ddir) return -ENOTDIR;
    if (sdir && ddir && !RemoveDirectoryW(dst)) return -weft_win_errno(GetLastError());
  }
  if (!MoveFileExW(src, dst, MOVEFILE_REPLACE_EXISTING)) return -last_err(dst);
  return 0;
}

static int64_t w_unlink(const wchar_t *p) {
  int t = trailing(p);
  if (t) return -t;
  if (DeleteFileW(p)) return 0;
  DWORD e = GetLastError();
  if (e == ERROR_ACCESS_DENIED && is_dir_attr(attrs_of(p))) return -EISDIR;
  return -fix_enoent(p, weft_win_errno(e));
}

/* POSIX refuses a last name of "." or "..": Windows would remove the
   directory. */
static int64_t w_rmdir(const wchar_t *p) {
  size_t n = wcslen(p);
  while (n > 1 && is_sep(p[n - 1])) n--;
  size_t b = n;
  while (b > 0 && !is_sep(p[b - 1])) b--;
  if ((n - b == 1 && p[b] == L'.') || (n - b == 2 && p[b] == L'.' && p[b + 1] == L'.'))
    return -EINVAL;
  if (RemoveDirectoryW(p)) return 0;
  return -last_err(p);
}

static void run(struct job *j) {
  int64_t r = 0;
  HANDLE h = (HANDLE)(intptr_t)j->fd;
  wchar_t *p = NULL, *p2 = NULL;
  if (j->kind == J_OPEN || j->kind == J_OPEN_STAT || j->kind == J_STAT || j->kind == J_MKDIR ||
      j->kind == J_READDIR || j->kind == J_RENAME || j->kind == J_UNLINK || j->kind == J_RMDIR) {
    p = weft_wide(j->s1);
    if (p == NULL) {
      fail(j, EINVAL);
      return;
    }
    if (j->kind == J_RENAME && (p2 = weft_wide(j->s2)) == NULL) {
      free(p);
      fail(j, EINVAL);
      return;
    }
  }
  switch (j->kind) {
  case J_OPEN:
    r = w_open(j, p);
    break;
  case J_PREAD:
    j->buf = malloc(j->a > 0 ? j->a : 1);
    if (j->buf == NULL) {
      r = -ENOMEM;
      break;
    }
    r = w_io(h, j->buf, j->a, j->b, 0);
    if (r >= 0) j->len = r;
    break;
  case J_PWRITE:
    r = w_io(h, j->buf, j->len, j->b, 1);
    break;
  case J_FSYNC:
    /* A handle with no write access has no data to flush. */
    if (!FlushFileBuffers(h) && GetLastError() != ERROR_ACCESS_DENIED)
      r = -weft_win_errno(GetLastError());
    break;
  case J_FSTAT: {
    BY_HANDLE_FILE_INFORMATION fi;
    if (GetFileInformationByHandle(h, &fi)) stat_of(j, &fi);
    else r = -weft_win_errno(GetLastError());
    break;
  }
  case J_OPEN_STAT: {
    /* If the stat fails, the job closes the handle and gives the error of the
       stat. */
    BY_HANDLE_FILE_INFORMATION fi;
    r = w_open(j, p);
    if (r < 0) break;
    if (GetFileInformationByHandle((HANDLE)(intptr_t)r, &fi)) {
      stat_of(j, &fi);
    } else {
      int e = weft_win_errno(GetLastError());
      CloseHandle((HANDLE)(intptr_t)r);
      r = -e;
    }
    break;
  }
  case J_STAT:
    r = w_stat_path(j, p);
    break;
  case J_FTRUNCATE: {
    FILE_END_OF_FILE_INFO eof;
    eof.EndOfFile.QuadPart = j->a;
    if (!SetFileInformationByHandle(h, FileEndOfFileInfo, &eof, sizeof eof))
      r = -weft_win_errno(GetLastError());
    break;
  }
  case J_MKDIR:
    if (!CreateDirectoryW(p, NULL)) r = -last_err(p);
    break;
  case J_READDIR:
    w_readdir(j, p);
    free(p);
    return;
  case J_RENAME:
    r = w_rename(p, p2);
    break;
  case J_UNLINK:
    r = w_unlink(p);
    break;
  case J_RMDIR:
    r = w_rmdir(p);
    break;
  case J_RESOLVE:
    resolve(j);
    return;
  case J_REVERSE:
    reverse(j);
    return;
  case J_CLOSE:
    if (!CloseHandle(h)) r = -EBADF;
    break;
  }
  free(p);
  free(p2);
  if (r < 0) fail(j, (int)-r);
  else {
    j->res = r;
    j->err = 0;
  }
}

