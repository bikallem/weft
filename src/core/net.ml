(* Sockets (design sections 2.3 and 2.7). Each operation is one [submit]. On
   [EAGAIN], the fiber waits for the next edge of the fd, then tries again. *)

open Sched

type addr = Op.addr = Tcp of string * int | Unix of string

type t = fdrec

type error = Op.error =
  | Would_block
  | Bad_fd
  | Refused
  | Reset
  | Broken_pipe
  | In_use
  | Other of string

exception Error of error * string

let submit name op = World.perform name (World.Submit op)

(* A [submit] holds a user of the record. In [Closing] or [Closed], it gets
   [EBADF] and adds no user (Eio's [Rcfd]). *)
let rec acquire fd name =
  match Atomic.get fd.rc with
  | Open n as old ->
      if not (Atomic.compare_and_set fd.rc old (Open (n + 1))) then
        acquire fd name
  | Closing _ | Closed ->
      raise (Error (Op.Bad_fd, name))

(* The side that changes [Closing 0] to [Closed] calls close(2), and only that
   side. *)
let rec release fd =
  match Atomic.get fd.rc with
  | Open n as old ->
      if not (Atomic.compare_and_set fd.rc old (Open (n - 1))) then release fd
  | Closing 1 as old ->
      if Atomic.compare_and_set fd.rc old Closed then (
        ignore (submit "Net.close" (Op.Close fd.os)) ;
        Fdtab.release fd )
      else release fd
  | Closing n as old ->
      if not (Atomic.compare_and_set fd.rc old (Closing (n - 1))) then
        release fd
  | Closed ->
      assert false

let attempt fd name op =
  acquire fd name ;
  match submit name (op fd.os) with
  | r ->
      release fd ; r
  | exception e ->
      release fd ; raise e

let await ~write fd =
  Event.sync
    (Event.Base
       (fun () -> {Event.leaf= fd_leaf ~write ~clear:true fd; result= ignore})
    )

(* Try until the operation does not give [EAGAIN]. *)
let rec retry ~write fd name op =
  match attempt fd name op with
  | Ok v ->
      v
  | Error Op.Would_block ->
      await ~write fd ; retry ~write fd name op
  | Error e ->
      raise (Error (e, name))

let readable fd =
  Event.Base
    (fun () ->
      {Event.leaf= fd_leaf ~write:false ~clear:false fd; result= ignore} )

let writable fd =
  Event.Base
    (fun () -> {Event.leaf= fd_leaf ~write:true ~clear:false fd; result= ignore})

let ok name = function Ok v -> v | Error e -> raise (Error (e, name))

(* [close] holds a user while it removes the registration, so close(2) comes
   after the Deregister (design section 2.7). A second [close] does nothing. *)
let close fd =
  match acquire fd "Net.close" with
  | exception Error _ ->
      ()
  | () ->
      let rec closing () =
        match Atomic.get fd.rc with
        | Open n as old ->
            if not (Atomic.compare_and_set fd.rc old (Closing n)) then
              closing ()
        | Closing _ | Closed ->
            assert false
      in
      closing () ;
      (* Milestone M0 has one domain, so the registration is on this domain. *)
      prog "Net.close" (Close_regs fd) ;
      release fd

let listen ?(backlog = 128) addr =
  Fdtab.alloc
    (ok "Net.listen" (submit "Net.listen" (Op.Listen (addr, backlog))))

let accept l =
  let os, peer = retry ~write:false l "Net.accept" (fun os -> Op.Accept os) in
  (Fdtab.alloc os, peer)

let connect addr =
  let fd =
    Fdtab.alloc (ok "Net.connect" (submit "Net.connect" (Op.Connect addr)))
  in
  match retry ~write:true fd "Net.connect" (fun os -> Op.Connect_result os) with
  | () ->
      fd
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      close fd ;
      Printexc.raise_with_backtrace e bt

let read fd buf off len =
  (* A read of 0 bytes would give 0, the same result as the end of the
     stream. *)
  if off < 0 || len < 1 || off > Bytes.length buf - len then
    invalid_arg "Net.read" ;
  retry ~write:false fd "Net.read" (fun os -> Op.Read (os, buf, off, len))

let write_sub fd buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Net.write_sub" ;
  retry ~write:true fd "Net.write" (fun os -> Op.Write (os, buf, off, len))

let write fd s =
  let b = Bytes.unsafe_of_string s in
  let rec loop off =
    if off < Bytes.length b then
      loop (off + write_sub fd b off (Bytes.length b - off))
  in
  loop 0

let shutdown fd =
  ok "Net.shutdown" (attempt fd "Net.shutdown" (fun os -> Op.Shutdown os))
