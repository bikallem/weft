(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The tests of the TLS flow that came from test/test_tls.ml of caravan (the
   group "The flow"). *)

open Windtrap
open Weft
open Weft_flow
open Support
module S = Weft_tls

let tests =
  group "The flow"
    [ test "read_now gives None for a part of a record" (fun () ->
          (* After [split := true], a write of 10 bytes or more goes in two
             parts, with 1 s between them. *)
          let split = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if len < 10 || not !split then Flow.write tcp buf off len
                else (
                  Flow.write tcp buf off (len / 2) ;
                  Time.sleep Mono (Span.s 1) ;
                  Flow.write tcp buf (off + (len / 2)) (len - (len / 2)) ) )
          in
          pair ~wrap (fun server client _ ->
              split := true ;
              (* No byte came: read_now does not wait. *)
              equal (option int) None
                (Flow.read_now server (Bytes.create 16) 0 16) ;
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () -> Flow.write_string client "hello") ;
                  (* At 0.5 s, half of the record came: no data yet. A read_now
                     that waits gives the data at 1 s. *)
                  Time.sleep Mono (Span.ms 500) ;
                  equal (option int) None
                    (Flow.read_now server (Bytes.create 16) 0 16) ;
                  equal string "hello" (read_string server 16) ) ) )
    ; test "shutdown_send sends close_notify: the peer reads 0" (fun () ->
          pair (fun server client _ ->
              Flow.write_string server "bye" ;
              Flow.shutdown_send server ;
              (* A second shutdown_send sends nothing: the flow can still
                 read. *)
              Flow.shutdown_send server ;
              Flow.write_string client "hi" ;
              (* The data and close_notify can come in one TCP read. *)
              Time.sleep Mono (Span.ms 100) ;
              equal string "bye" (read_string client 16) ;
              equal int 0 (Flow.read client (Bytes.create 1) 0 1) ;
              equal (option int) (Some 0)
                (Flow.read_now client (Bytes.create 1) 0 1) ;
              equal string "hi" (read_string server 16) ) )
    ; test "a TCP end with no close_notify raises Closed" (fun () ->
          pair (fun server client tcp ->
              Flow.shutdown_send tcp ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              (* Each later read raises again. *)
              raises_closed (fun () -> ignore (read_string server 16)) ;
              (* The peer can still read: a write works. *)
              Flow.write_string server "still" ;
              equal string "still" (read_string client 16) ) )
    ; test "a TCP end with no close_notify, seen by read_now" (fun () ->
          pair (fun server _ tcp ->
              Flow.shutdown_send tcp ;
              Time.sleep Mono (Span.ms 100) ;
              raises_closed (fun () ->
                  ignore (Flow.read_now server (Bytes.create 4) 0 4) ) ) )
    ; test "a bad record after the handshake raises Closed" (fun () ->
          pair (fun server client tcp ->
              (* The plaintext before the bad record comes first. *)
              Flow.write_string client "ok" ;
              Time.sleep Mono (Span.ms 100) ;
              Flow.write_string tcp bad_record ;
              Time.sleep Mono (Span.ms 100) ;
              equal string "ok" (read_string server 16) ;
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "a record header with no body raises Closed" (fun () ->
          (* An empty record is 5 bytes: the split must give it to the session,
             which refuses it. *)
          pair (fun server _ tcp ->
              Flow.write_string tcp "\x17\x03\x03\x00\x00" ;
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "a record header of 64 KiB fails at once" (fun () ->
          (* ocaml-tls refuses a record of more than 18 KiB, thus the flow gives
             the header to it at once and does not wait for 64 KiB. *)
          pair (fun server _ tcp ->
              Flow.write_string tcp "\x17\x03\x03\xff\xff" ;
              raises_closed (fun () -> ignore (read_string server 16)) ) ;
          (* A data record, then the header, in one TCP write. *)
          let scfg, ccfg = setup () in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = raw_handshake tcp ccfg in
                  let server = Promise.await server in
                  let _, out =
                    Option.get (Tls.Engine.send_application_data tls ["x"])
                  in
                  Flow.write_string tcp (out ^ "\x17\x03\x03\xff\xff") ;
                  equal string "x" (read_string server 16) ;
                  raises_closed (fun () -> ignore (read_string server 16)) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) )
    ; test "KeyUpdates in the write of the Finished message count" (fun () ->
          (* [updates n] sends the Finished message, [n] KeyUpdates and "x" in
             one TCP write: the records after the handshake go to the budget of
             the session. *)
          let scfg, ccfg = setup () in
          let updates n =
            run (fun _ ->
                let l = listen () in
                Fiber.scope (fun s ->
                    let server =
                      Fiber.fork_promise s (fun () ->
                          let c, _ = Net.accept l in
                          let tcp = Tcp.flow c in
                          Fun.protect ~finally:(fun () -> Flow.close tcp)
                          @@ fun () ->
                          match S.server scfg tcp with
                          | f, _ ->
                              read_string f 16
                          | exception S.Handshake_error m ->
                              m )
                    in
                    let tcp = connect () in
                    let with_finished tls =
                      let rec go tls n acc =
                        if n = 0 then
                          let tls, out =
                            Option.get
                              (Tls.Engine.send_application_data tls ["x"])
                          in
                          (tls, String.concat "" (List.rev (out :: acc)))
                        else
                          match Tls.Engine.key_update ~request:false tls with
                          | Ok (tls, out) ->
                              go tls (n - 1) (out :: acc)
                          | Error f ->
                              fail (Tls.Engine.string_of_failure f)
                      in
                      go tls n []
                    in
                    ignore (raw_handshake ~with_finished tcp ccfg) ;
                    let r = Promise.await server in
                    Flow.close tcp ; Net.close l ; r ) )
          in
          equal string "x" (updates 64) ;
          equal string "too many records with no plaintext" (updates 65) )
    ; test "a handshake of exactly 64 KiB starts a session" (fun () ->
          (* The ClientHello, ChangeCipherSpec records and the Finished message
             have 65536 bytes in all. ocaml-tls ignores the body of a
             ChangeCipherSpec record in the handshake. *)
          let scfg, ccfg = setup () in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server = accept_one s scfg l in
                  let tcp = connect () in
                  let tls, hello = Tls.Engine.client ccfg in
                  Flow.write_string tcp hello ;
                  let buf = Bytes.create 65536 in
                  let rec finished tls =
                    let n = Flow.read tcp buf 0 65536 in
                    match
                      Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n)
                    with
                    | Ok (tls, _, `Response (Some fin), _)
                      when not (Tls.Engine.handshake_in_progress tls) ->
                        fin
                    | Ok (tls, _, _, _) ->
                        finished tls
                    | Error (f, _) ->
                        fail (Tls.Engine.string_of_failure f)
                  in
                  let fin = finished tls in
                  (* Four records with a body of 16 KiB or less. *)
                  let pad = 65536 - String.length hello - String.length fin in
                  let body = (pad - 20) / 4 in
                  let ccs n =
                    Printf.sprintf "\x14\x03\x03%c%c%s"
                      (Char.chr (n lsr 8))
                      (Char.chr (n land 255))
                      (String.make n '\x01')
                  in
                  let last = pad - 20 - (3 * body) in
                  is_true ~msg:"the size of a record" (last <= 16384) ;
                  Flow.write_string tcp
                    (ccs body ^ ccs body ^ ccs body ^ ccs last ^ fin) ;
                  equal string "a session" (Promise.await server) ;
                  Flow.close tcp ;
                  Net.close l ) ) )
    ; test "records after close_notify, and no empty record" (fun () ->
          let scfg, ccfg = setup () in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = raw_handshake tcp ccfg in
                  let server = Promise.await server in
                  (* "a", close_notify and "b" in one TCP write: the flow
                     ignores "b". *)
                  let tls, a =
                    Option.get (Tls.Engine.send_application_data tls ["a"])
                  in
                  let tls, close = Tls.Engine.send_close_notify tls in
                  let _, b =
                    match Tls.Engine.send_application_data tls ["b"] with
                    | Some x ->
                        x
                    | None ->
                        (* ocaml-tls can refuse data after its close_notify:
                           then the record of "a" again stands for "b". *)
                        (tls, a)
                  in
                  Flow.write_string tcp (a ^ close ^ b) ;
                  equal string "a" (read_string server 16) ;
                  equal int 0 (Flow.read server (Bytes.create 4) 0 4) ;
                  (* A write of 16 KiB makes no empty record: ocaml-tls 2.1.3
                     adds one to a write of exactly 16384 bytes. An empty record
                     of TLS 1.3 has 17 bytes: the type and the tag. *)
                  Flow.write_string server (String.make 16384 'w') ;
                  Flow.shutdown_send server ;
                  let wire = read_all tcp in
                  let rec lengths i acc =
                    if i + 5 > String.length wire then acc
                    else
                      let len =
                        (Char.code wire.[i + 3] lsl 8)
                        lor Char.code wire.[i + 4]
                      in
                      lengths (i + 5 + len) (len :: acc)
                  in
                  is_false ~msg:"an empty record" (List.mem 17 (lengths 0 [])) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) )
    ; test "shutdown_send ends the TCP stream when close_notify fails"
        (fun () ->
          let fail_write = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if !fail_write then raise Flow.Timeout
                else Flow.write tcp buf off len )
          in
          pair ~wrap (fun server client _ ->
              fail_write := true ;
              raises_match
                (function Flow.Timeout -> true | _ -> false)
                (fun () -> Flow.shutdown_send client) ;
              (* The TCP end came, with no close_notify. *)
              raises_closed (fun () -> ignore (read_string server 16)) ) )
    ; test "readable: buffered data, a failed flow, and no busy loop" (fun () ->
          (* After [deaf := true], the TCP flow of the server is never readable:
             then only the buffer and the state of the TLS flow make it
             readable. *)
          let deaf = ref false in
          let swrap tcp =
            fake_tcp tcp
              ~readable:
                (guard (fun () -> if !deaf then never else Flow.readable tcp))
          in
          pair ~swrap (fun server client tcp ->
              (* A wait for data that comes at 1 s: readiness can be stale, but
                 a readable that is always ready makes a busy loop. *)
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      Time.sleep Mono (Span.s 1) ;
                      Flow.write_string client "z" ) ;
                  let rec wait n =
                    if n > 100 then fail "a busy loop on readable" ;
                    sync (Flow.readable server) ;
                    match Flow.read_now server (Bytes.create 4) 0 4 with
                    | Some _ ->
                        ()
                    | None ->
                        wait (n + 1)
                  in
                  wait 0 ) ;
              Flow.write_string client "abcdef" ;
              equal string "ab" (read_string server 2) ;
              deaf := true ;
              is_true ~msg:"buffered" (ready server) ;
              equal string "cdef" (read_string server 16) ;
              Flow.write_string tcp bad_record ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              is_true ~msg:"failed" (ready server) ) )
    ; test "a write that fails in a record fails the flow" (fun () ->
          (* After [fail_write := true], the next write sends half of its bytes,
             then raises Timeout. *)
          let fail_write = ref false in
          let wrap tcp =
            fake_tcp tcp ~write:(fun buf off len ->
                if !fail_write then (
                  fail_write := false ;
                  Flow.write tcp buf off (len / 2) ;
                  raise Flow.Timeout )
                else Flow.write tcp buf off len )
          in
          pair ~wrap (fun _ client _ ->
              fail_write := true ;
              raises_match
                (function Flow.Timeout -> true | _ -> false)
                (fun () -> Flow.write_string client "first") ;
              (* Half of a record went out: no record can follow it. *)
              raises_closed (fun () -> Flow.write_string client "second") ) )
    ; test "a write after close_notify, and after a failed write" (fun () ->
          pair (fun server client tcp ->
              Flow.shutdown_send server ;
              raises_closed (fun () -> Flow.write_string server "late") ;
              (* A write of the client on a closed TCP flow fails it. *)
              Flow.close tcp ;
              raises_closed (fun () -> Flow.write_string client "x") ;
              raises_closed (fun () -> ignore (read_string client 16)) ;
              (* shutdown_send of a failed flow raises nothing. *)
              Flow.shutdown_send client ) )
    ; test "a reader never waits for a TCP write, on 4 domains" (fun () ->
          (* The client sends 1 MiB, and reads only after that. The server reads
             in one fiber, and sends 1 MiB in another one. The socket of the
             simulator holds 64 KiB: the write of the server waits until the
             client reads. If the reader of the server waited for that write,
             the upload of the client would never end. *)
          let size = 1024 * 1024 in
          let data = String.init size (fun i -> Char.chr (i mod 253)) in
          pair ~domains:4 (fun server client _ ->
              Fiber.scope (fun s ->
                  let got =
                    Fiber.fork_promise s (fun () -> read_n server size)
                  in
                  Fiber.fork s (fun () -> Flow.write_string server data) ;
                  Flow.write_string client data ;
                  is_true ~msg:"client" (String.equal data (read_n client size)) ;
                  is_true ~msg:"server" (String.equal data (Promise.await got)) ) ) )
    ; test "a peer that leaves or stops in the handshake" (fun () ->
          let _, other =
            setup
              ~peer_name:Domain_name.(host_exn (of_string_exn "example.org"))
              ()
          in
          let scfg, ccfg = setup () in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server = accept_one s scfg l in
                  Flow.close (connect ()) ;
                  equal string "the peer closed the connection"
                    (Promise.await server) ;
                  (* A client that refuses the certificate sends an alert before
                     it closes. *)
                  let server = accept_one s scfg l in
                  let tcp = connect () in
                  raises_match
                    (function S.Handshake_error _ -> true | _ -> false)
                    (fun () -> ignore (S.client other tcp)) ;
                  Flow.close tcp ;
                  equal string "alert bad certificate" (Promise.await server) ;
                  (* A close_notify alert in place of a ClientHello. *)
                  let server = accept_one s scfg l in
                  let tcp = connect () in
                  Flow.write_string tcp "\x15\x03\x03\x00\x02\x01\x00" ;
                  equal string "the peer closed the session"
                    (Promise.await server) ;
                  Flow.close tcp ;
                  (* A ClientHello, then a plain alert: TLS 1.3 refuses it. *)
                  let server = accept_one s scfg l in
                  let _, hello = Tls.Engine.client ccfg in
                  let tcp = connect () in
                  Flow.write_string tcp (hello ^ "\x15\x03\x03\x00\x02\x01\x00") ;
                  is_true ~msg:"an alert after the hello"
                    (Promise.await server <> "a session") ;
                  Flow.close tcp ;
                  (* A bad record, then a reset: the alert of the server finds
                     no peer. *)
                  let server = accept_one ~pause:(Span.ms 100) s scfg l in
                  let tcp = connect () in
                  Flow.write_string tcp "\x16\x03\x01\x00\x05hello" ;
                  Flow.reset tcp ;
                  is_true ~msg:"garbage" (Promise.await server <> "a session") ;
                  Net.close l ) ) )
    ; test "a KeyUpdate of the peer during a response" (fun () ->
          (* A raw client asks for a KeyUpdate in the middle of a 1 MiB
             response. The server must send its KeyUpdate before its next data:
             if not, the client cannot read the data. *)
          let scfg, ccfg = setup () in
          let size = 1024 * 1024 in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        let f, _ = S.server scfg (Tcp.flow c) in
                        ignore (read_string f 16) ;
                        (* A reader fiber gives the KeyUpdate to the session
                           while the server writes. *)
                        Fiber.scope (fun s ->
                            Fiber.fork s (fun () ->
                                match read_string f 16 with
                                | _ ->
                                    ()
                                | exception Flow.Closed ->
                                    () ) ;
                            for _ = 1 to 64 do
                              Flow.write_string f (String.make (size / 64) 'k') ;
                              Time.sleep Mono (Span.ms 1)
                            done ;
                            Flow.shutdown_send f ) ;
                        f )
                  in
                  let tcp = connect () in
                  let tls = send tcp (raw_handshake tcp ccfg) "go" in
                  let updated = ref false in
                  let on_data tls got =
                    if got > size / 4 && not !updated then (
                      updated := true ;
                      match Tls.Engine.key_update ~request:true tls with
                      | Ok (tls, out) ->
                          Flow.write_string tcp out ; tls
                      | Error f ->
                          fail (Tls.Engine.string_of_failure f) )
                    else tls
                  in
                  let got = raw_read ~on_data tcp tls in
                  is_true ~msg:"a KeyUpdate" !updated ;
                  equal int size (String.length got) ;
                  Flow.close tcp ;
                  Flow.close (Promise.await server) ;
                  Net.close l ) ) )
    ; test "records with no plaintext have a budget" (fun () ->
          (* 64 KeyUpdates pass. More than the budget fail the flow: ocaml-tls
             keeps memory for each one. *)
          let scfg, ccfg = setup () in
          run (fun _ ->
              let l = listen () in
              Fiber.scope (fun s ->
                  let server =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        fst (S.server scfg (Tcp.flow c)) )
                  in
                  let tcp = connect () in
                  let tls = ref (raw_handshake tcp ccfg) in
                  let server = Promise.await server in
                  let updates n =
                    for _ = 1 to n do
                      match Tls.Engine.key_update ~request:false !tls with
                      | Ok (t, out) ->
                          tls := t ;
                          Flow.write_string tcp out
                      | Error f ->
                          fail (Tls.Engine.string_of_failure f)
                    done
                  in
                  updates 64 ;
                  tls := send tcp !tls "a" ;
                  equal string "a" (read_string server 16) ;
                  (* With "a", 32 KiB of plaintext give 2 more. 32767 bytes:
                     ocaml-tls adds an empty record to 32768. *)
                  let data = String.make 32767 'p' in
                  tls := send tcp !tls data ;
                  equal int 32767 (String.length (read_n server 32767)) ;
                  updates 2 ;
                  tls := send tcp !tls "b" ;
                  equal string "b" (read_string server 16) ;
                  updates 1 ;
                  tls := send tcp !tls "c" ;
                  raises_closed (fun () -> ignore (read_string server 16)) ;
                  Flow.close tcp ;
                  Flow.close server ;
                  Net.close l ) ) ) ]
