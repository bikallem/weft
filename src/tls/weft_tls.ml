(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft
module Flow = Weft_flow.Flow

exception Handshake_error of string

(* The size of the buffer of the TCP reads: more than the largest record of TLS
   1.3 (16645 bytes). *)
let read_size = 16384 + 512

(* The most bytes that a handshake reads, before the authentication of the peer.
   ocaml-tls keeps a handshake message until all of it comes, and a message can
   declare 16 MiB. *)
let max_handshake = 65536

(* The records with no plaintext (for example a KeyUpdate or a ChangeCipherSpec)
   that a session permits: [64], and one more for each 16 KiB of plaintext.
   ocaml-tls 2.1.3 adds a session to its state for each KeyUpdate, and a record
   with no plaintext makes no progress for the application. *)
let empty_budget plain = 64 + (plain / 16384)

(* [cost r] is the cost of the record [r] with no plaintext in the budget: 1,
   and 1 more for each KiB. A part of a handshake message after the handshake
   (ocaml-tls keeps it until the message ends, and a message can declare 16 MiB)
   thus costs as much as its size. *)
let cost r =
  ( 1
  + (String.length r / 1024) )
  [@mutate
    off
      "the API of ocaml-tls cannot send a large record with no plaintext: the \
       budget tests use small records"]

(* The largest record that ocaml-tls accepts: 16 KiB and 2 KiB. A longer record
   goes to handle_tls at once, which refuses it: the carry does not grow to 64
   KiB. *)
let max_record = 16384 + 2048

(* The ciphertext that the session did not take: [buf] from 0, [len] bytes. A
   TCP read puts its bytes at the end of [buf] directly. After [take_records],
   [len] is less than [5 + max_record], thus [buf] has space for [read_size]
   bytes or more. Without this buffer, a record that comes in k TCP reads gives
   k copies of the part that came before each read: a peer that sends small TCP
   segments then costs time quadratic in the size of the record. *)
type carry = {buf: bytes; mutable len: int}

let new_carry () = {buf= Bytes.create (5 + max_record + read_size); len= 0}

(* [read_into read c] reads into the space of [c] with [read], and gives the
   count. *)
let read_into read c = read c.buf c.len (Bytes.length c.buf - c.len)

(* [record_end c i] is the end of the record at the offset [i] of [c], if [c]
   has all of it. For a record that is too long, it is the end of [c]: the
   session refuses the record at once, thus [c] does not wait for 64 KiB. *)
let record_end c i =
  if c.len - i < 5 then None
  else
    let len =
      (Bytes.get_uint8 c.buf (i + 3) lsl 8) lor Bytes.get_uint8 c.buf (i + 4)
    in
    if (len > max_record) [@mutate off "the exact bound of a limit"] then
      Some c.len
    else if c.len - i < 5 + len then None
    else Some (i + 5 + len)

(* [complete c] tells that [c] starts with a record that can go to the
   session. *)
let complete c = Option.is_some (record_end c 0)

(* [take_records c] removes the complete records at the start of [c] and gives
   them. Each record goes to handle_tls alone: ocaml-tls copies the rest of its
   input for each record of the input. *)
let take_records c =
  let rec go i acc =
    match record_end c i with
    | Some j ->
        go j (Bytes.sub_string c.buf i (j - i) :: acc)
    | None ->
        Bytes.blit c.buf i c.buf 0 (c.len - i) ;
        c.len <- c.len - i ;
        List.rev acc
  in
  go 0 []

(* The state of a TLS flow. It has two locks. The state lock covers the pure
   calls of ocaml-tls, [tls] and [pending], and no sync occurs in it. The write
   lock covers a write of the TCP flow, thus the records go out in the order of
   their sequence numbers.

   The reader never writes. A response of handle_tls (for example a KeyUpdate)
   waits in [pending] for the next write (RFC 8446 §4.6.3). Thus a reader never
   waits for a TCP write, and [read_now] never waits for input. But [read_now]
   can wait for the state lock while a writer encrypts one record, thus a cancel
   can come in it. Only the reader uses [carry], [plain], [pos], [plain_total]
   and [empty]. [status] is atomic: a failure stays.

   A read that raises takes no byte (the rule of Flow.make). The bytes of a TCP
   read go to [carry] before the reader takes the state lock. Thus a cancel at
   the lock keeps them, and the next read gives them to the session before it
   reads the TCP flow. *)
type t =
  { tcp: Flow.t
  ; mutable tls: Tls.Engine.state
  ; mutable pending: string list  (** The responses, the last one first. *)
  ; carry: carry
  ; mutable plain: string  (** Plaintext that the application did not read. *)
  ; mutable pos: int  (** The first byte of [plain] for the application. *)
  ; mutable plain_total: int
  ; mutable empty: int  (** The records with no plaintext. *)
  ; status: [`Open | `End | `Failed] Atomic.t
  ; mutable notified: bool  (** A close_notify went out. *)
  ; state_lock: Fiber.Mutex.t
  ; write_lock: Fiber.Mutex.t }

(* [with_lock m f] is [f ()] in [m]. [Fiber.Mutex.use_rw] poisons [m] if [f]
   raises, and a write that raises Flow.Closed is a usual event. [use_ro] does
   not poison, but it is for code that only reads. *)
let with_lock m f =
  Fiber.Mutex.lock m ;
  Fun.protect ~finally:(fun () -> Fiber.Mutex.unlock m) f

let fail t = Atomic.set t.status `Failed

let failed t = Atomic.get t.status = `Failed

(* [reraise_failed t f] is [f ()]. If [f] raises, also for a cancel, the flow is
   failed: a part of a record can be lost or sent. *)
let reraise_failed t f =
  match f () with
  | v ->
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      fail t ;
      Printexc.raise_with_backtrace e bt

(* [feed t] gives the complete records of [carry] to the session, one record at
   a time. After the handshake, a failure only marks the flow: the reader never
   writes, thus no alert goes out. A cancel at the lock raises before the split:
   [carry] and [unfed] keep the bytes for the next read. *)
let feed t =
  with_lock t.state_lock
  @@ fun () ->
  let records = take_records t.carry in
  let data =
    reraise_failed t (fun () ->
        List.fold_left
          (fun data r ->
            (* After close_notify or a failure, no more records. *)
            if Atomic.get t.status <> `Open then data
            else
              match Tls.Engine.handle_tls t.tls r with
              | Ok (tls, eof, `Response resp, `Data d) -> (
                  t.tls <- tls ;
                  Option.iter (fun r -> t.pending <- r :: t.pending) resp ;
                  if Option.is_some eof then
                    ignore (Atomic.compare_and_set t.status `Open `End) ;
                  match d with
                  | Some d ->
                      (* The plaintext of this read is in the budget of the
                         records that follow it. *)
                      t.plain_total <- t.plain_total + String.length d ;
                      d :: data
                  | None ->
                      t.empty <- t.empty + cost r ;
                      if t.empty > empty_budget t.plain_total then fail t ;
                      data )
              | Error _ ->
                  fail t ; data )
          [] records )
  in
  (* The reader feeds only when it took all of [plain]. *)
  t.plain <- String.concat "" (List.rev data) ;
  t.pos <- 0

(* [take t buf off len] gives the buffered plaintext: [Some n], [Some 0] at the
   end, or [None] if the session needs more ciphertext. The plaintext of the
   peer is authenticated, thus a failure raises [Flow.Closed] only after it. *)
let take t buf off len =
  let avail = String.length t.plain - t.pos in
  if avail > 0 then (
    let n = min len avail in
    Bytes.blit_string t.plain t.pos buf off n ;
    if n = avail then (
      t.plain <- "" ;
      t.pos <-
        (0
        [@mutate
          off
            "[plain] is empty: each [pos] of 0 or more gives no plaintext, and \
             [feed] sets [pos] again"] ) )
    else t.pos <- t.pos + n ;
    Some n )
  else
    match Atomic.get t.status with
    | `End ->
        Some 0
    | `Failed ->
        raise Flow.Closed
    | `Open ->
        None

(* The end of the TCP stream with no close_notify: a truncation. Each later read
   finds the end again. *)
let truncated () = raise Flow.Closed

(* [read] loops until plaintext comes. The budget of records with no plaintext
   ends the loop. A complete record in [carry], also one that a cancel left
   there, goes to the session before the next TCP read. *)
let rec read t buf off len =
  match take t buf off len with
  | Some n ->
      n
  | None when complete t.carry ->
      feed t ; read t buf off len
  | None -> (
    match read_into (Flow.read t.tcp) t.carry with
    | 0 ->
        truncated ()
    | n ->
        t.carry.len <- t.carry.len + n ;
        read t buf off len )

(* [read_now] reads the TCP flow one time: input with no plaintext gives [None],
   thus its caller examines its deadlines again. *)
let read_now t buf off len =
  match take t buf off len with
  | Some n ->
      Some n
  | None ->
      ( if not (complete t.carry) then
          match read_into (Flow.read_now t.tcp) t.carry with
          | None ->
              ()
          | Some 0 ->
              truncated ()
          | Some n ->
              t.carry.len <- t.carry.len + n ) ;
      if complete t.carry then (feed t ; take t buf off len) else None

let readable t =
  guard (fun () ->
      if
        String.length t.plain > t.pos
        || complete t.carry
        || Atomic.get t.status <> `Open
      then always ()
      else Flow.readable t.tcp )

(* [send t make] takes the records of [make tls] and the pending responses, and
   writes them in the write lock. A write that raises, also for a cancel, can
   leave a part of a record: the flow is then failed. *)
let send t make =
  with_lock t.write_lock (fun () ->
      let out =
        with_lock t.state_lock (fun () ->
            if failed t then raise Flow.Closed ;
            match make t.tls with
            | None ->
                raise Flow.Closed
            | Some (tls, out) ->
                t.tls <- tls ;
                let p = t.pending in
                t.pending <- [] ;
                if
                  (p = []) [@mutate off "the same bytes, with no copy of [out]"]
                then out
                else String.concat "" (List.rev_append p [out]) )
      in
      reraise_failed t (fun () -> Flow.write_string t.tcp out) )

(* [write] encrypts one record at a time, thus a reader waits for the state lock
   only for the encryption of one record. *)
let write t buf off len =
  let rec go off len =
    if len > 0 then (
      (* ocaml-tls 2.1.3 adds an empty record to a write of exactly 16 KiB. *)
      let k = min len 16383 in
      send t (fun tls ->
          Tls.Engine.send_application_data tls [Bytes.sub_string buf off k] ) ;
      go (off + k) (len - k) )
  in
  go off len

(* [notify t] sends close_notify, one time. *)
let notify t =
  if not t.notified then (
    t.notified <- true ;
    send t (fun tls -> Some (Tls.Engine.send_close_notify tls)) )

(* [shutdown_send t] sends close_notify, then the end of the TCP stream, also
   when the close_notify fails. *)
let shutdown_send t =
  match notify t with
  | () | (exception Flow.Closed) ->
      Flow.shutdown_send t.tcp
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Flow.shutdown_send t.tcp ;
      Printexc.raise_with_backtrace e bt

let flow_of t =
  Flow.make ~read:(read t) ~read_now:(read_now t) ~write:(write t)
    ~readable:(readable t)
    ~shutdown_send:(fun () -> shutdown_send t)
    ~close:(fun () -> Flow.close t.tcp)
    ~reset:(fun () -> Flow.reset t.tcp)
    ~set_read_deadline:(Flow.set_read_deadline t.tcp)
    ~set_write_deadline:(Flow.set_write_deadline t.tcp)
    ~set_read_timeout:(Flow.set_read_timeout t.tcp)
    ~set_write_timeout:(Flow.set_write_timeout t.tcp)
    ()

(* [establish tcp tls] reads [tcp] until the handshake of [tls] ends, and gives
   the flow of the session and its data. The reads have the deadlines and the
   timeouts of [tcp]. One fiber does the handshake, thus it writes each
   response, and the alert of a failure. The records that come after the end of
   the handshake go to the session: their plaintext is in the buffer of the
   flow. *)
let establish tcp tls =
  let carry = new_carry () in
  let rec loop tls plain eof total empty =
    if (not (Tls.Engine.handshake_in_progress tls)) || eof then
      (tls, plain, eof, empty)
    else
      match read_into (Flow.read tcp) carry with
      | 0 ->
          raise (Handshake_error "the peer closed the connection")
      | n ->
          let total = total + n in
          if total > max_handshake then
            raise (Handshake_error "the handshake is too large") ;
          carry.len <- carry.len + n ;
          let records = take_records carry in
          let tls, plain, eof, empty =
            List.fold_left
              (fun (tls, plain, eof, empty) r ->
                if eof then (tls, plain, eof, empty)
                else
                  (* A record with no plaintext after the end of the handshake
                     counts in the budget of the session. Over the budget, the
                     session does not start. *)
                  let after = not (Tls.Engine.handshake_in_progress tls) in
                  match Tls.Engine.handle_tls tls r with
                  | Ok (tls, e, `Response resp, `Data d) ->
                      Option.iter (Flow.write_string tcp) resp ;
                      let plain, empty =
                        match d with
                        | Some d ->
                            (d :: plain, empty)
                        | None when after ->
                            let empty = empty + cost r in
                            let bytes =
                              List.fold_left
                                (fun n d -> n + String.length d)
                                (0
                                [@mutate
                                  off
                                    "the budget changes only at 16383 bytes, \
                                     and then the 16896 bytes of one read have \
                                     no space for 65 records"] )
                                plain
                            in
                            if empty > empty_budget bytes then
                              raise
                                (Handshake_error
                                   "too many records with no plaintext" ) ;
                            (plain, empty)
                        | None ->
                            (plain, empty)
                      in
                      (tls, plain, Option.is_some e, empty)
                  | Error (failure, `Response alert) ->
                      ( match Flow.write_string tcp alert with
                      | () ->
                          ()
                      | exception (Flow.Closed | Flow.Timeout) ->
                          () ) ;
                      raise
                        (Handshake_error (Tls.Engine.string_of_failure failure)) )
              (tls, plain, eof, empty) records
          in
          loop tls plain eof total empty
  in
  let tls, plain, eof, empty = loop tls [] false 0 0 in
  let epoch =
    match Tls.Engine.epoch tls with
    | Ok e ->
        e
    | Error () ->
        raise (Handshake_error "the peer closed the session")
  in
  let plain = String.concat "" (List.rev plain) in
  ( { tcp
    ; tls
    ; pending= []
    ; carry
    ; plain
    ; pos= 0
    ; plain_total= String.length plain
    ; empty
    ; status= Atomic.make (if eof then `End else `Open)
    ; notified= false
    ; state_lock= Fiber.Mutex.create ()
    ; write_lock= Fiber.Mutex.create () }
  , epoch )

(* [check_rng name] raises [Invalid_argument] for the function [name] if no
   default generator is set. Without the check, the first handshake raises
   No_default_generator from the depth of ocaml-tls. *)
let check_rng name =
  match Mirage_crypto_rng.default_generator () with
  | _ ->
      ()
  | exception Mirage_crypto_rng.No_default_generator ->
      invalid_arg
        ( "Weft_tls." ^ name
        ^ ": no default generator: call Mirage_crypto_rng_unix.use_default () \
           first" )

let server config tcp =
  check_rng "server" ;
  let t, epoch = establish tcp (Tls.Engine.server config) in
  (flow_of t, epoch)

let client config tcp =
  check_rng "client" ;
  let tls, hello = Tls.Engine.client config in
  Flow.write_string tcp hello ;
  let t, epoch = establish tcp tls in
  (flow_of t, epoch)
