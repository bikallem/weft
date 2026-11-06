(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The suite of weft.flow on the simulator (design section 2.18): the group
   "Flow" of test/test_core.ml of caravan, and the runner. The tests of Tcp are
   in test_tcp.ml. *)

open Windtrap
open Weft_flow

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

(* Flow *)

let read_all = Helpers.read_all

(* [readable_before_20ms flow] is [true] if [readable flow] is ready before 20
   ms of virtual time. *)
let readable_before_20ms flow =
  Weft.(
    sync
      (choose
         [ wrap (Flow.readable flow) (fun () -> true)
         ; wrap (Time.after Mono (Span.ms 20)) (fun () -> false) ] ) )

(* A flow for the tests of [Flow]. Each function that the test does not give
   does nothing: a read gives 0 (the end of the stream), and [readable] is ready
   at once. *)
let fake_flow ?(read = fun _ _ _ -> 0) ?(read_now = fun _ _ _ -> Some 0)
    ?(write = fun _ _ _ -> ()) ?(readable = Weft.always ())
    ?(shutdown_send = ignore) ?(close = ignore) ?reset ?set_read_timeout
    ?set_write_timeout () =
  Flow.make ~read ~read_now ~write ~readable ~shutdown_send ~close ?reset
    ~set_read_deadline:ignore ~set_write_deadline:ignore ?set_read_timeout
    ?set_write_timeout ()

(* [timeout_text name d] is the log line of a set of the timeout [name] to
   [d]. *)
let timeout_text name = function
  | None ->
      name ^ " none"
  | Some d ->
      Format.asprintf "%s %a" name Weft.Span.pp d

(* A fake flow that logs the sets of its timeouts in [log]. *)
let logged_flow log =
  let note s = log := s :: !log in
  fake_flow
    ~set_read_timeout:(fun d -> note (timeout_text "rt" d))
    ~set_write_timeout:(fun d -> note (timeout_text "wt" d))
    ()

let flow_tests =
  group "Flow"
    [ test "of_string reads the input, then 0 for good" (fun () ->
          let flow = Flow.of_string "hello" (Buffer.create 0) in
          equal string "hello" (read_all flow) ;
          equal int 0 (Flow.read flow (Bytes.create 4) 0 4) )
    ; test "chunk caps each read" (fun () ->
          let flow = Flow.of_string ~chunk:2 "hello" (Buffer.create 0) in
          let buf = Bytes.create 8 in
          equal (list int) [2; 2; 1; 0]
            (List.init 4 (fun _ -> Flow.read flow buf 0 8)) )
    ; test "writes append to the output" (fun () ->
          let out = Buffer.create 16 in
          let flow = Flow.of_string "" out in
          Flow.write flow (Bytes.of_string "xxabcxx") 2 3 ;
          Flow.write_string flow "def" ;
          Flow.write flow (Bytes.of_string "z") 0 0 ;
          equal string "abcdef" (Buffer.contents out) )
    ; test "invalid ranges are rejected" (fun () ->
          let flow = Flow.of_string "abc" (Buffer.create 0) in
          let buf = Bytes.create 4 in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () -> Flow.read flow buf off len) )
            [(0, 0); (-1, 2); (0, 5); (3, 2)] ;
          raises_match is_invalid_arg (fun () -> Flow.write flow buf 2 3) ;
          raises_match is_invalid_arg (fun () ->
              Flow.of_string ~chunk:0 "" (Buffer.create 0) ) )
    ; test "read_now reads what is there without blocking" (fun () ->
          let buf = Bytes.create 4 in
          let f = Flow.of_string ~chunk:2 "abc" (Buffer.create 0) in
          equal
            (list (option int))
            [Some 2; Some 1; Some 0]
            (List.init 3 (fun _ -> Flow.read_now f buf 0 4)) ;
          let nothing =
            fake_flow
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ -> None)
              ~readable:Weft.never ()
          in
          let p = Flow.prepend "x" nothing in
          equal
            (list (option int))
            [Some 1; None]
            (List.init 2 (fun _ -> Flow.read_now p buf 0 4)) ;
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Flow.read_now f buf off len ) )
            [(0, 0); (-1, 2); (3, 2)] ;
          Flow.close f ;
          raises Flow.Closed (fun () -> Flow.read_now f buf 0 1) )
    ; test "reset closes once, abortively, and close falls back to it"
        (fun () ->
          let log = ref [] in
          let flow ?reset () =
            fake_flow ~close:(fun () -> log := "close" :: !log) ?reset ()
          in
          let f = flow ~reset:(fun () -> log := "reset" :: !log) () in
          Flow.reset f ;
          Flow.reset f ;
          Flow.close f ;
          equal (list string) ["reset"] !log ;
          raises Flow.Closed (fun () -> Flow.read f (Bytes.create 1) 0 1) ;
          log := [] ;
          Flow.reset (flow ()) ;
          equal (list string) ~msg:"without reset, close" ["close"] !log )
    ; test "write rejects an invalid range before the write of the flow"
        (fun () ->
          let writes = ref 0 in
          let flow = fake_flow ~write:(fun _ _ _ -> incr writes) () in
          let buf = Bytes.create 4 in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Flow.write flow buf off len ) )
            [(-1, 2); (0, 5); (3, 2); (2, -1)] ;
          equal int ~msg:"no write of the flow" 0 !writes )
    ; test "make delegates to its functions" (fun () ->
          let log = ref [] in
          let note s = log := s :: !log in
          let flow =
            Flow.make
              ~read:(fun buf off _ -> note "read" ; Bytes.set buf off 'a' ; 1)
              ~read_now:(fun _ _ _ -> note "read_now" ; None)
              ~write:(fun _ _ len -> note (Printf.sprintf "write %d" len))
              ~readable:(Weft.always ())
              ~shutdown_send:(fun () -> note "shutdown")
              ~close:(fun () -> note "close")
              ~set_read_deadline:(fun d ->
                note (if d = None then "rd none" else "rd") )
              ~set_write_deadline:(fun d ->
                note (if d = None then "wd none" else "wd") )
              ~set_read_timeout:(fun d -> note (timeout_text "rt" d))
              ~set_write_timeout:(fun d -> note (timeout_text "wt" d))
              ()
          in
          equal int 1 (Flow.read flow (Bytes.create 1) 0 1) ;
          equal (option int) None (Flow.read_now flow (Bytes.create 1) 0 1) ;
          Flow.write_string flow "abc" ;
          Flow.write_string flow "" ;
          Flow.shutdown_send flow ;
          Flow.set_read_deadline flow None ;
          Flow.set_write_deadline flow None ;
          Flow.set_read_timeout flow (Some (Weft.Span.s 5)) ;
          Flow.set_write_timeout flow (Some (Weft.Span.ms 7)) ;
          Flow.set_read_timeout flow None ;
          Flow.set_write_timeout flow None ;
          Flow.close flow ;
          equal (list string)
            [ "read"
            ; "read_now"
            ; "write 3"
            ; "shutdown"
            ; "rd none"
            ; "wd none"
            ; "rt 5000000000ns"
            ; "wt 7000000ns"
            ; "rt none"
            ; "wt none"
            ; "close" ]
            (List.rev !log) )
    ; test "prepend of an empty string is the flow" (fun () ->
          let f = Flow.of_string "a" (Buffer.create 0) in
          is_true (Flow.prepend "" f == f) )
    ; test "prepend reads its bytes first" (fun () ->
          let out = Buffer.create 8 in
          let flow =
            Flow.prepend "ab" (Flow.prepend "" (Flow.of_string "cd" out))
          in
          equal string "abcd" (read_all ~size:3 flow) ;
          Flow.write_string flow "w" ;
          equal string "w" (Buffer.contents out) ;
          equal string "xyz"
            (read_all
               (Flow.prepend "x" (Flow.prepend "y" (Flow.of_string "z" out))) ) )
    ; test "prepend passes shutdown and close to the flow" (fun () ->
          let log = ref [] in
          let flow =
            fake_flow
              ~shutdown_send:(fun () -> log := "shutdown" :: !log)
              ~close:(fun () -> log := "close" :: !log)
              ()
          in
          let p = Flow.prepend "x" flow in
          Flow.shutdown_send p ;
          Flow.close p ;
          equal (list string) ["shutdown"; "close"] (List.rev !log) ;
          let f = Flow.of_string "" (Buffer.create 0) in
          let now = Helpers.run (fun _ -> Weft.Time.now Mono) in
          Flow.set_read_deadline f (Some now) ;
          Flow.set_write_deadline f None ;
          Flow.shutdown_send f ;
          equal int 0 (Flow.read f (Bytes.create 1) 0 1) )
    ; test "prepend gives the timeouts to the flow" (fun () ->
          let log = ref [] in
          let p = Flow.prepend "x" (logged_flow log) in
          Flow.set_read_timeout p (Some (Weft.Span.s 1)) ;
          Flow.set_write_timeout p (Some (Weft.Span.s 2)) ;
          equal (list string)
            ["rt 1000000000ns"; "wt 2000000000ns"]
            (List.rev !log) )
    ; test "prepend is readable while its bytes remain" (fun () ->
          let flow = Flow.prepend "a" (fake_flow ~readable:Weft.never ()) in
          Helpers.manual
          @@ fun sim ->
          Weft.Fiber.scope
          @@ fun s ->
          let c = Helpers.call s (fun () -> readable_before_20ms flow) in
          is_true ~msg:"ready at once before the prefix is read"
            (Helpers.ended sim c && Helpers.await c) ;
          equal int 1 (Flow.read flow (Bytes.create 4) 0 4) ;
          let t0 = Weft.(Time.now Mono) in
          let c = Helpers.call s (fun () -> readable_before_20ms flow) in
          equal bool ~msg:"waits for the inner flow after" false
            (Helpers.ends_at sim ~t0 (Weft.Span.ms 20) c) )
    ; prop "reads return the input however it is split"
        Gen.(
          pair
            (pair string (int_range 0 64))
            (pair (int_range 1 9) (int_range 1 9)) )
        (fun ((input, cut), (chunk, size)) ->
          let cut = Int.min cut (String.length input) in
          let flow =
            Flow.prepend (String.sub input 0 cut)
              (Flow.of_string ~chunk
                 (String.sub input cut (String.length input - cut))
                 (Buffer.create 0) )
          in
          equal string input (read_all ~size flow) ) ]

let () = exit (Windtrap.run "flow" (flow_tests :: Test_tcp.tests))
