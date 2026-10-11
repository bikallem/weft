(* Pipes (design section 2.3, The I/O modules). The two ends are non-blocking
   fds in the poller, as sockets are. *)

type t = Sched.fdrec

exception Error = Net.Error

let create () =
  let r, w = Net.ok "Pipe.create" (Net.submit "Pipe.create" Op.Pipe) in
  (Sched.Fdtab.alloc r, Sched.Fdtab.alloc w)

let read fd buf off len =
  Net.check_range ~read:true "Pipe.read" buf off len ;
  Net.after_read fd
    (Net.retry ~write:false fd "Pipe.read" (fun os ->
         Op.Fd_read (os, buf, off, len) ) )

let write_sub fd buf off len =
  Net.check_range ~read:false "Pipe.write_sub" buf off len ;
  Net.retry ~write:true fd "Pipe.write" (fun os ->
      Op.Fd_write (os, buf, off, len) )

let write = Net.write_all write_sub

let close = Net.close
