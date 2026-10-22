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
  | No_resources of string
  | Other of string

exception Error of error * string

(* The name of the constructor, as in the source text. *)
let error_name = function
  | Would_block ->
      "Would_block"
  | Bad_fd ->
      "Bad_fd"
  | Refused ->
      "Refused"
  | Reset ->
      "Reset"
  | Broken_pipe ->
      "Broken_pipe"
  | In_use ->
      "In_use"
  | No_entry ->
      "No_entry"
  | Exists ->
      "Exists"
  | Not_dir ->
      "Not_dir"
  | Is_dir ->
      "Is_dir"
  | Not_empty ->
      "Not_empty"
  | No_resources s ->
      Printf.sprintf "No_resources %S" s
  | Other s ->
      Printf.sprintf "Other %S" s

(* With no printer, an uncaught error shows the number of the constructor (issue
   38). [Pipe.Error], [Process.Error], [Fs] and [Dns] use the same exception. *)
let () =
  Printexc.register_printer (function
    | Error (e, name) ->
        Some (Printf.sprintf "Weft.Net.Error(%s, %S)" (error_name e) name)
    | _ ->
        None )

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
      fd.streak <- 0 ;
      Event.sync (fd_event ~write ~clear:true fd) ;
      retry ~write fd name op
  | Error e ->
      raise (Error (e, name))

(* A ready event after a close: the next operation gives [EBADF]. *)
let readable fd = fd_event ~write:false ~clear:false fd

let writable fd = fd_event ~write:true ~clear:false fd

let ok name = function Ok v -> v | Error e -> raise (Error (e, name))

(* The checks of the strings and the ports that go to the OS, one time for all
   worlds (design section 2.3). The C side reads a string up to its first NUL
   byte, and [Int_val] to an [int] of C cuts a large port. Thus without these
   checks, the OS can use a different path or address. *)
let check_string name s =
  if String.contains s '\000' then
    raise (Error (Op.Other "EINVAL: a NUL byte in a string", name))

let check_host_port name (host, port) =
  check_string name host ;
  if port < 0 || port > 65535 then raise (Error (Op.Other "EINVAL", name))

let check_addr name = function
  | Tcp (host, port) ->
      check_host_port name (host, port)
  | Unix path ->
      check_string name path

(* [close] never blocks. If users remain, each domain with a registration gets a
   close notice, and the last user calls close(2) (design section 2.7,
   spec/tla/Fd.tla). A second close does nothing. [Self] first: outside a fiber,
   [close] raises [Not_in_fiber] before it changes the state. *)
let close fd =
  ignore (self ()) ;
  if Rc.close fd then prog "Net.close" (Close_notices fd)

let listen ?(backlog = 128) addr =
  check_addr "Net.listen" addr ;
  Fdtab.alloc
    (ok "Net.listen" (submit "Net.listen" (Op.Listen (addr, backlog))))

let accept l =
  let os, peer = retry ~write:false l "Net.accept" (fun os -> Op.Accept os) in
  (Fdtab.alloc os, peer)

let connect addr =
  check_addr "Net.connect" addr ;
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

let local_addr fd =
  ok "Net.local_addr" (attempt fd "Net.local_addr" (fun os -> Op.Local_addr os))

(* The range of a read or a write. A read needs at least one byte: a read of 0
   bytes would give 0, the same result as the end of the stream. *)
let check_range ~read name buf off len =
  if off < 0 || (len < if read then 1 else 0) || off > Bytes.length buf - len
  then invalid_arg name

(* Write all of [s] with a function that can write a part. *)
let write_all write_sub fd s =
  let b = Bytes.unsafe_of_string s in
  let rec loop off =
    if off < Bytes.length b then
      loop (off + write_sub fd b off (Bytes.length b - off))
  in
  loop 0

(* A reader whose reads always find data never switches (design section 2.7).
   After [yield_bytes] bytes with no wait, a read yields. The yield is not a
   cancellation point: the read already took the bytes. *)
let yield_bytes = ref (64 * 1024)

let after_read fd n =
  fd.streak <- fd.streak + n ;
  if (fd.streak >= !yield_bytes) [@mutate off "the limit or one byte more"] then (
    fd.streak <- 0 ;
    ignore (prog "Net.read" Yield) ) ;
  n

let read fd buf off len =
  check_range ~read:true "Net.read" buf off len ;
  after_read fd
    (retry ~write:false fd "Net.read" (fun os -> Op.Read (os, buf, off, len)))

let write_sub fd buf off len =
  check_range ~read:false "Net.write_sub" buf off len ;
  retry ~write:true fd "Net.write" (fun os -> Op.Write (os, buf, off, len))

let write = write_all write_sub

(* One attempt with no wait. On [EAGAIN], the flag of the domain is stale, as in
   [retry]. Thus the next [readable] or [writable] waits for a new edge, and a
   loop of [readable] and [read_now] does not spin. *)
let now ~write fd name op =
  match attempt fd name op with
  | Ok v ->
      Some v
  | Error Op.Would_block ->
      fd.streak <- 0 ;
      prog name (Clear_ready (fd, write)) ;
      None
  | Error e ->
      raise (Error (e, name))

let read_now fd buf off len =
  check_range ~read:true "Net.read_now" buf off len ;
  Option.map (after_read fd)
    (now ~write:false fd "Net.read_now" (fun os -> Op.Read (os, buf, off, len)))

let write_now fd buf off len =
  check_range ~read:false "Net.write_now" buf off len ;
  now ~write:true fd "Net.write_now" (fun os -> Op.Write (os, buf, off, len))

let shutdown fd =
  ok "Net.shutdown" (attempt fd "Net.shutdown" (fun os -> Op.Shutdown os))

(* The option of the reset is a use of the fd, as a read: it holds a user. Then
   [close] does the close protocol, and the close(2) of the last user sends the
   reset. If the record is already closing, or if the OS refuses the option, the
   close is a normal close. I think that macOS refuses an option on a connection
   that ended in the two directions. *)
let reset fd =
  ignore (self ()) ;
  ( match attempt fd "Net.reset" (fun os -> Op.Reset_on_close os) with
  | _ ->
      ()
  | exception Error (Op.Bad_fd, _) ->
      () ) ;
  close fd

(* UDP (design section 2.3, The I/O modules). A datagram moves in one operation,
   or not at all. *)
let udp (host, port) =
  check_host_port "Net.udp" (host, port) ;
  Fdtab.alloc ~kind:Udp (ok "Net.udp" (submit "Net.udp" (Op.Udp (host, port))))

(* On Linux, a sendto on a TCP socket ignores the address, and it can send a
   part. Thus the two worlds refuse it before the submit. *)
let check_udp name fd =
  if fd.fkind <> Udp then invalid_arg (name ^ ": not a UDP socket")

let sendto fd buf off len (host, port) =
  check_range ~read:false "Net.sendto" buf off len ;
  check_udp "Net.sendto" fd ;
  (* Before the submit: a closed socket also gives this error. *)
  check_host_port "Net.sendto" (host, port) ;
  ignore
    (retry ~write:true fd "Net.sendto" (fun os ->
         Op.Sendto (os, buf, off, len, host, port) ) )

let recvfrom fd buf off len =
  check_range ~read:true "Net.recvfrom" buf off len ;
  check_udp "Net.recvfrom" fd ;
  let n, host, port =
    retry ~write:false fd "Net.recvfrom" (fun os ->
        Op.Recvfrom (os, buf, off, len) )
  in
  (* An empty datagram counts as one byte: a reader of empty datagrams also
     yields. *)
  ignore (after_read fd (Int.max n 1)) ;
  (n, (host, port))

(* Socket options. *)
type 'a sockopt = 'a Op.sockopt =
  | No_delay : bool sockopt
  | Keep_alive : bool sockopt
  | Broadcast : bool sockopt
  | Recv_buffer : int sockopt
  | Send_buffer : int sockopt

(* A size is an [int] of C: the limit keeps a large size large. The OS lowers it
   to its own limit. *)
let check_size v =
  if v < 1 then invalid_arg "Net.set_option: the size is not positive" ;
  Int.min v 0x7fff_ffff

let set_option (type a) fd (opt : a sockopt) (v : a) =
  let v : a =
    match opt with
    | Recv_buffer ->
        check_size v
    | Send_buffer ->
        check_size v
    | No_delay | Keep_alive | Broadcast ->
        v
  in
  ok "Net.set_option"
    (attempt fd "Net.set_option" (fun os -> Op.Set_option (os, opt, v)))

let get_option fd opt =
  ok "Net.get_option"
    (attempt fd "Net.get_option" (fun os -> Op.Get_option (os, opt)))
