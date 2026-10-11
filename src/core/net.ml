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
  | No_entry
  | Exists
  | Not_dir
  | Is_dir
  | Not_empty
  | Other of string

exception Error of error * string

let submit name op = World.perform name (World.Submit op)

(* A [submit] holds a user of the record. In [Closing] or [Closed], it gets
   [EBADF] and adds no user (Eio's [Rcfd]). *)
let acquire fd name = if not (Rc.acquire fd) then raise (Error (Op.Bad_fd, name))

let release = Rc.release

let attempt fd name op =
  acquire fd name ;
  match submit name (op fd.os) with
  | r ->
      release fd ; r
  | exception e ->
      release fd ; raise e

(* The event of a wait for an fd. If the Register of the fd fails, the event
   raises the error. *)
let fd_event ~write ~clear fd =
  Event.Base
    (fun () ->
      let err = ref None in
      { Event.leaf= fd_leaf ~write ~clear ~err fd
      ; result=
          (fun () ->
            match !err with Some e -> raise (Error (e, "Net")) | None -> () ) } )

(* Try until the operation does not give [EAGAIN]. *)
let rec retry ~write fd name op =
  match attempt fd name op with
  | Ok v ->
      v
  | Error Op.Would_block ->
      Event.sync (fd_event ~write ~clear:true fd) ;
      retry ~write fd name op
  | Error e ->
      raise (Error (e, name))

(* A ready event after a close: the next operation gives [EBADF]. *)
let readable fd = fd_event ~write:false ~clear:false fd

let writable fd = fd_event ~write:true ~clear:false fd

let ok name = function Ok v -> v | Error e -> raise (Error (e, name))

(* [close] never blocks. If users remain, each domain with a registration gets a
   close notice, and the last user calls close(2) (design section 2.7,
   spec/tla/Fd.tla). A second close does nothing. [Self] first: outside a fiber,
   [close] raises [Not_in_fiber] before it changes the state. *)
let close fd =
  ignore (self ()) ;
  if Rc.close fd then prog "Net.close" (Close_notices fd)

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
