(* Pipes (design section 2.3, The I/O modules). The two ends are non-blocking
   fds in the poller, as sockets are. *)

type t = Sched.fdrec

exception Error = Net.Error

let create () =
  let r, w = Net.ok "Pipe.create" (Net.submit "Pipe.create" Op.Pipe) in
  (Sched.Fdtab.alloc ~kind:Read_end r, Sched.Fdtab.alloc ~kind:Write_end w)

(* A read of a write end, or a write of a read end, gives EBADF as on Linux. *)
let direction name fd wrong =
  if fd.Sched.fkind = wrong then raise (Error (Op.Bad_fd, name))

let read fd buf off len =
  Net.check_range ~read:true "Pipe.read" buf off len ;
  direction "Pipe.read" fd Write_end ;
  Net.after_read fd
    (Net.retry ~write:false fd "Pipe.read" (fun os ->
         Op.Fd_read (os, buf, off, len) ) )

let write_sub fd buf off len =
  Net.check_range ~read:false "Pipe.write_sub" buf off len ;
  direction "Pipe.write" fd Read_end ;
  Net.retry ~write:true fd "Pipe.write" (fun os ->
      Op.Fd_write (os, buf, off, len) )

let write = Net.write_all write_sub

let close = Net.close
