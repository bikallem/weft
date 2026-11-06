(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

exception Timeout

exception Closed

type t =
  { read: bytes -> int -> int -> int
  ; read_now: bytes -> int -> int -> int option
  ; write: bytes -> int -> int -> unit
  ; readable: unit Weft.event
  ; shutdown_send: unit -> unit
  ; close: unit -> unit
  ; reset: unit -> unit
  ; set_read_deadline: Weft.Clock.mono option -> unit
  ; set_write_deadline: Weft.Clock.mono option -> unit
  ; set_read_timeout: Weft.Span.t option -> unit
  ; set_write_timeout: Weft.Span.t option -> unit
  ; closed: bool Atomic.t }

let make ~read ~read_now ~write ~readable ~shutdown_send ~close ?(reset = close)
    ~set_read_deadline ~set_write_deadline ?(set_read_timeout = ignore)
    ?(set_write_timeout = ignore) () =
  { read
  ; read_now
  ; write
  ; readable
  ; shutdown_send
  ; close
  ; reset
  ; set_read_deadline
  ; set_write_deadline
  ; set_read_timeout
  ; set_write_timeout
  ; closed= Atomic.make false }

let check_range fn buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg (Printf.sprintf "Flow.%s: invalid range" fn)

let check_read fn flow buf off len =
  check_range fn buf off len ;
  if len = 0 then invalid_arg (Printf.sprintf "Flow.%s: empty read" fn) ;
  if Atomic.get flow.closed then raise Closed

let read flow buf off len =
  check_read "read" flow buf off len ;
  flow.read buf off len

let read_now flow buf off len =
  check_read "read_now" flow buf off len ;
  flow.read_now buf off len

let readable flow = flow.readable

let write flow buf off len =
  check_range "write" buf off len ;
  if Atomic.get flow.closed then raise Closed ;
  if len > 0 then flow.write buf off len

let write_string flow s =
  write flow (Bytes.unsafe_of_string s) 0 (String.length s)

let shutdown_send flow =
  if not (Atomic.get flow.closed) then flow.shutdown_send ()

let set_read_deadline flow d = flow.set_read_deadline d

let set_write_deadline flow d = flow.set_write_deadline d

let set_read_timeout flow span = flow.set_read_timeout span

let set_write_timeout flow span = flow.set_write_timeout span

let close flow = if not (Atomic.exchange flow.closed true) then flow.close ()

let reset flow = if not (Atomic.exchange flow.closed true) then flow.reset ()

let prepend s flow =
  if s = "" then flow
  else begin
    let pos = ref 0 in
    let remaining () = String.length s - !pos in
    let take buf off len =
      let n = Int.min len (remaining ()) in
      Bytes.blit_string s !pos buf off n ;
      pos := !pos + n ;
      n
    in
    let read buf off len =
      if remaining () = 0 then read flow buf off len else take buf off len
    in
    let read_now buf off len =
      if remaining () = 0 then read_now flow buf off len
      else Some (take buf off len)
    in
    let readable =
      Weft.guard (fun () ->
          if remaining () > 0 then Weft.always () else flow.readable )
    in
    make ~read ~read_now ~write:(write flow) ~readable
      ~shutdown_send:(fun () -> shutdown_send flow)
      ~close:(fun () -> close flow)
      ~reset:(fun () -> reset flow)
      ~set_read_deadline:(set_read_deadline flow)
      ~set_write_deadline:(set_write_deadline flow)
      ~set_read_timeout:(set_read_timeout flow)
      ~set_write_timeout:(set_write_timeout flow) ()
  end

let of_string ?(chunk = max_int) input output =
  if chunk <= 0 then invalid_arg "Flow.of_string: chunk must be positive" ;
  let pos = ref 0 in
  let read buf off len =
    let n = Int.min (Int.min len chunk) (String.length input - !pos) in
    Bytes.blit_string input !pos buf off n ;
    pos := !pos + n ;
    n
  in
  let write buf off len = Buffer.add_subbytes output buf off len in
  make ~read
    ~read_now:(fun buf off len -> Some (read buf off len))
    ~write ~readable:(Weft.always ()) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore ~set_write_deadline:ignore ()
