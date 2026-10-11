(* Files and directories (design section 2.3, The I/O modules). Each operation
   is a job of the pool. *)

module File = struct
  exception Error = Net.Error

  type t = Sched.fdrec

  type flag = Read | Write | Create | Exclusive | Truncate | Append

  let op_flag = function
    | Read ->
        Op.O_read
    | Write ->
        Op.O_write
    | Create ->
        Op.O_create
    | Exclusive ->
        Op.O_exclusive
    | Truncate ->
        Op.O_truncate
    | Append ->
        Op.O_append

  type kind = Op.kind = Regular | Directory | Symlink | Special

  type stat = {kind: kind; size: int; perm: int; mtime: Clock.posix}

  let stat_of (s : Op.stat) =
    {kind= s.kind; size= s.size; perm= s.perm; mtime= Clock.of_ns Posix s.mtime}

  let openfile ?(perm = 0o644) flags path =
    Sched.Fdtab.alloc
      (Job.run "File.openfile" (Op.Open (path, List.map op_flag flags, perm)))

  let check name buf off len =
    if off < 0 || len < 1 || off > Bytes.length buf - len then invalid_arg name

  let read fd buf off len =
    check "File.read" buf off len ;
    Job.run ~fd "File.read" (Op.Pread (fd.Sched.os, buf, off, len, -1))

  let pread fd ~file_offset buf off len =
    check "File.pread" buf off len ;
    if file_offset < 0 then invalid_arg "File.pread" ;
    Job.run ~fd "File.pread" (Op.Pread (fd.Sched.os, buf, off, len, file_offset))

  (* The world copies the data before the job starts (design section 2.3). *)
  let write_at name fd file_offset buf off len =
    if off < 0 || len < 0 || off > Bytes.length buf - len then invalid_arg name ;
    Job.run ~fd name
      (Op.Pwrite (fd.Sched.os, Bytes.sub_string buf off len, file_offset))

  let write_sub fd buf off len = write_at "File.write" fd (-1) buf off len

  let pwrite fd ~file_offset buf off len =
    if file_offset < 0 then invalid_arg "File.pwrite" ;
    write_at "File.pwrite" fd file_offset buf off len

  let write fd s =
    let b = Bytes.unsafe_of_string s in
    let rec loop off =
      if off < Bytes.length b then
        loop (off + write_sub fd b off (Bytes.length b - off))
    in
    loop 0

  let fsync fd = Job.run ~fd "File.fsync" (Op.Fsync fd.Sched.os)

  let fstat fd = stat_of (Job.run ~fd "File.fstat" (Op.Fstat fd.Sched.os))

  let stat path = stat_of (Job.run "File.stat" (Op.Stat path))

  let truncate fd size =
    if size < 0 then invalid_arg "File.truncate" ;
    Job.run ~fd "File.truncate" (Op.Ftruncate (fd.Sched.os, size))

  let close = Net.close
end

module Dir = struct
  exception Error = Net.Error

  let mkdir ?(perm = 0o755) path = Job.run "Dir.mkdir" (Op.Mkdir (path, perm))

  (* The order of the OS is not fixed, so the names are sorted. *)
  let readdir path =
    List.sort String.compare (Job.run "Dir.readdir" (Op.Readdir path))

  let rename src dst = Job.run "Dir.rename" (Op.Rename (src, dst))

  let unlink path = Job.run "Dir.unlink" (Op.Unlink path)

  let rmdir path = Job.run "Dir.rmdir" (Op.Rmdir path)
end

module Dns = struct
  exception Error = Net.Error

  let resolve host = Job.run "Dns.resolve" (Op.Resolve host)

  let reverse addr = Job.run "Dns.reverse" (Op.Reverse addr)
end
