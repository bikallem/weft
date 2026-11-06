(* The dscheck tests of the TLS session (design section 2.19). Each test runs
   each order of the atomic operations of its threads: the locks of the session,
   its status, and the cancel of the model. *)

module Flow = Weft_flow.Flow

let failures = ref 0

let traces = ref 0

let pruned_traces = ref 0

let pruned = ref false

(* Run a test: [setup] spawns the threads and gives the check of the end. *)
let trace setup =
  Atomic.trace (fun () ->
      Domain.spins := 0 ;
      pruned := false ;
      let check = setup () in
      Atomic.final (fun () ->
          incr traces ;
          if !pruned then incr pruned_traces else Atomic.check check ) )

(* A thread of a test. A spin limit drops its trace. *)
let spawn f =
  Atomic.spawn (fun () -> try f () with Domain.Pruned -> pruned := true)

let case name f =
  traces := 0 ;
  pruned_traces := 0 ;
  match f () with
  | () when !traces = !pruned_traces ->
      incr failures ;
      Printf.printf "  [FAIL] %s: each trace was pruned\n%!" name
  | () ->
      Printf.printf "  [OK]   %s (%d traces, %d pruned)\n%!" name !traces
        !pruned_traces
  | exception e ->
      incr failures ;
      Printf.printf "  [FAIL] %s: %s\n%!" name (Printexc.to_string e)

(* {1 A session} *)

(* [handshake ()] is the state of a client and of a server of ocaml-tls after a
   handshake in memory. The states are values, thus each trace starts from them,
   with no crypto of a handshake. *)
let handshake () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"weft dscheck tls"
       (module Mirage_crypto_rng.Fortuna) ) ;
  let time () = Ptime.of_float_s 1_750_000_000. in
  let scfg, ccfg = Tls_support.configs ~time () in
  let handle st s =
    match Tls.Engine.handle_tls st s with
    | Ok (st, _, `Response r, _) ->
        (st, Option.value r ~default:"")
    | Error (f, _) ->
        failwith (Tls.Engine.string_of_failure f)
  in
  let rec go c s to_server =
    if Tls.Engine.handshake_in_progress c || Tls.Engine.handshake_in_progress s
    then
      let s, to_client =
        if to_server = "" then (s, "") else handle s to_server
      in
      let c, to_server =
        if to_client = "" then (c, "") else handle c to_client
      in
      go c s to_server
    else (c, s)
  in
  let c, hello = Tls.Engine.client ccfg in
  go c (Tls.Engine.server scfg) hello

let client0, server0 = handshake ()

(* [session ~input] is a TLS flow of the server state, on a base flow whose
   first read gives [input], and whose writes go to the buffer that it gives. A
   second read with no input would wait: the test fails. *)
let session ~input =
  let out = Buffer.create 256 and given = ref false in
  let read buf off len =
    Weft.Fiber.check_cancel () ;
    if !given then failwith "a read that waits"
    else (
      given := true ;
      let n = String.length input in
      assert (n <= len) ;
      Bytes.blit_string input 0 buf off n ;
      n )
  in
  let tcp =
    Flow.make ~read
      ~read_now:(fun _ _ _ -> None)
      ~write:(fun b off len -> Buffer.add_subbytes out b off len)
      ~readable:(fun () -> None)
      ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
      ~set_write_deadline:ignore ()
  in
  let t =
    { Weft_tls.tcp
    ; tls= server0
    ; pending= []
    ; carry= Weft_tls.new_carry ()
    ; plain= ""
    ; pos= 0
    ; plain_total= 0
    ; empty= 0
    ; status= Atomic.make `Open
    ; notified= false
    ; state_lock= Weft.Fiber.Mutex.create ()
    ; write_lock= Weft.Fiber.Mutex.create () }
  in
  (Weft_tls.flow_of t, out)

(* [retry f] is [f ()], again after each cancel. *)
let rec retry f =
  match f () with v -> v | exception Weft.Fiber.Cancelled _ -> retry f

(* [read_text flow] reads the flow one time, and gives the text, or the name of
   its exception. *)
let read_text flow =
  let buf = Bytes.create 64 in
  match retry (fun () -> Flow.read flow buf 0 64) with
  | n ->
      Bytes.sub_string buf 0 n
  | exception Flow.Closed ->
      "Closed"
  | exception Failure m ->
      m

(* [write_text flow s] writes [s], again after each cancel, and gives "ok" or
   the name of its exception. *)
let write_text flow s =
  match retry (fun () -> Flow.write_string flow s) with
  | () ->
      "ok"
  | exception Flow.Closed ->
      "Closed"

(* [decode client wire] is the plaintext that the peer [client] reads from the
   writes of the session, or the text of its failure. *)
let decode client wire =
  match Tls.Engine.handle_tls client wire with
  | Ok (_, _, _, `Data d) ->
      Option.value d ~default:""
  | Error (f, _) ->
      Tls.Engine.string_of_failure f

let encrypt tls data = Option.get (Tls.Engine.send_application_data tls [data])

(* {1 The tests} *)

(* A reader, a writer and a cancel. The cancel can come at the read of the base
   flow, at the state lock in the read, or at a lock of the write. The read
   after a cancel gives the plaintext of the record: the cancel took no byte
   (the read rule of Weft_flow.Flow). *)
let cancel_and_read () =
  let client, record = encrypt client0 "r" in
  trace (fun () ->
      Weft.Fiber.cancel := Atomic.make false ;
      let flow, out = session ~input:record in
      let got = ref "" and wrote = ref "" in
      spawn (fun () -> got := read_text flow) ;
      spawn (fun () -> wrote := write_text flow "w") ;
      spawn (fun () -> Atomic.set !Weft.Fiber.cancel true) ;
      fun () ->
        !got = "r" && !wrote = "ok" && decode client (Buffer.contents out) = "w" )

(* A KeyUpdate of the peer that asks for an update, and a write at the same
   time. The response of the reader goes out before the next record of the
   writer, thus the peer reads each record in order. *)
let key_update_and_write () =
  let client, update =
    match Tls.Engine.key_update ~request:true client0 with
    | Ok x ->
        x
    | Error f ->
        failwith (Tls.Engine.string_of_failure f)
  in
  let client, record = encrypt client "r" in
  trace (fun () ->
      Weft.Fiber.cancel := Atomic.make false ;
      let flow, out = session ~input:(update ^ record) in
      let got = ref "" and wrote = ref "" in
      spawn (fun () -> got := read_text flow) ;
      spawn (fun () -> wrote := write_text flow "w") ;
      fun () ->
        !got = "r" && !wrote = "ok" && decode client (Buffer.contents out) = "w" )

let () =
  print_endline "dscheck tls" ;
  case "a cancel of the reader, and a write" cancel_and_read ;
  case "a KeyUpdate of the peer, and a write" key_update_and_write ;
  if !failures > 0 then exit 1
