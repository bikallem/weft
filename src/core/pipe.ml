(* Pipes (design section 2.3, The I/O modules). The two ends are non-blocking
   fds in the poller, as sockets are. *)

type t = Sched.fdrec

exception Error = Net.Error

let create () =
  match Net.submit "Pipe.create" Op.Pipe with
  | Ok (r, w) ->
      (Sched.Fdtab.alloc r, Sched.Fdtab.alloc w)
  | Error e ->
      raise (Error (e, "Pipe.create"))

let read fd buf off len =
  if off < 0 || len < 1 || off > Bytes.length buf - len then
    invalid_arg "Pipe.read" ;
  Net.retry ~write:false fd "Pipe.read" (fun os ->
      Op.Fd_read (os, buf, off, len) )

let write_sub fd buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Pipe.write_sub" ;
  Net.retry ~write:true fd "Pipe.write" (fun os ->
      Op.Fd_write (os, buf, off, len) )

let write fd s =
  let b = Bytes.unsafe_of_string s in
  let rec loop off =
    if off < Bytes.length b then
      loop (off + write_sub fd b off (Bytes.length b - off))
  in
  loop 0

let close = Net.close
