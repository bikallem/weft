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

(* [split s] is the complete records at the start of [s], and the rest: a part
   of a record. Each record goes to handle_tls alone: ocaml-tls copies the rest
   of its input for each record of the input. *)
let split s =
  let n = String.length s in
  let rec go i acc =
    if n - i < 5 then (List.rev acc, String.sub s i (n - i))
    else
      let len = (Char.code s.[i + 3] lsl 8) lor Char.code s.[i + 4] in
      if (len > max_record) [@mutate off "the exact bound of a limit"] then
        (List.rev (String.sub s i (n - i) :: acc), "")
      else if n - i < 5 + len then (List.rev acc, String.sub s i (n - i))
      else go (i + 5 + len) (String.sub s i (5 + len) :: acc)
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
   and [empty]. [status] is atomic: a failure stays. *)
type t =
  { tcp: Flow.t
  ; cbuf: bytes  (** The buffer of the TCP reads. *)
  ; mutable tls: Tls.Engine.state
  ; mutable pending: string list  (** The responses, the last one first. *)
  ; mutable carry: string  (** A part of a record. *)
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

(* [feed t s] gives the ciphertext [s] to the session, one record at a time.
   After the handshake, a failure only marks the flow: the reader never writes,
   thus no alert goes out. *)
let feed t s =
  let records, rest = split (t.carry ^ s) in
  t.carry <- rest ;
  let data =
    reraise_failed t (fun () ->
        with_lock t.state_lock (fun () ->
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
              [] records ) )
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
   ends the loop. *)
let rec read t buf off len =
  match take t buf off len with
  | Some n ->
      n
  | None -> (
    match Flow.read t.tcp t.cbuf 0 read_size with
    | 0 ->
        truncated ()
    | n ->
        feed t (Bytes.sub_string t.cbuf 0 n) ;
        read t buf off len )

(* [read_now] reads the TCP flow one time: input with no plaintext gives [None],
   thus its caller examines its deadlines again. *)
let read_now t buf off len =
  match take t buf off len with
  | Some n ->
      Some n
  | None -> (
    match Flow.read_now t.tcp t.cbuf 0 read_size with
    | None ->
        None
    | Some 0 ->
        truncated ()
    | Some n ->
        feed t (Bytes.sub_string t.cbuf 0 n) ;
        take t buf off len )

let readable t =
  guard (fun () ->
      if String.length t.plain > t.pos || Atomic.get t.status <> `Open then
        always ()
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

(* [establish ~read tcp tls] reads with [read] until the handshake of [tls]
   ends, and gives the flow of the session and its data. One fiber does the
   handshake, thus it writes each response, and the alert of a failure. The
   records that come after the end of the handshake go to the session: their
   plaintext is in the buffer of the flow. *)
let establish ~read tcp tls =
  let cbuf = Bytes.create read_size in
  let rec loop tls carry plain eof total empty =
    if (not (Tls.Engine.handshake_in_progress tls)) || eof then
      (tls, carry, plain, eof, empty)
    else
      match read cbuf with
      | 0 ->
          raise (Handshake_error "the peer closed the connection")
      | n ->
          let total = total + n in
          if total > max_handshake then
            raise (Handshake_error "the handshake is too large") ;
          let records, carry = split (carry ^ Bytes.sub_string cbuf 0 n) in
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
          loop tls carry plain eof total empty
  in
  let tls, carry, plain, eof, empty = loop tls "" [] false 0 0 in
  let epoch =
    match Tls.Engine.epoch tls with
    | Ok e ->
        e
    | Error () ->
        raise (Handshake_error "the peer closed the session")
  in
  let plain = String.concat "" (List.rev plain) in
  ( { tcp
    ; cbuf
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

(* The public handshakes read with the deadlines of the flow. The engine has a
   read of its own, with the stop of the server. *)
let read_with_deadlines tcp b = Flow.read tcp b 0 (Bytes.length b)

let server config tcp =
  let t, epoch =
    establish ~read:(read_with_deadlines tcp) tcp (Tls.Engine.server config)
  in
  (flow_of t, epoch)

let client config tcp =
  let tls, hello = Tls.Engine.client config in
  Flow.write_string tcp hello ;
  let t, epoch = establish ~read:(read_with_deadlines tcp) tcp tls in
  (flow_of t, epoch)
