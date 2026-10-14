(* The tests of the I/O modules above the worlds that the mutation run asked for
   (design sections 2.3 and 2.6). *)

open Windtrap
open Weft
module Sim = Weft_sim
module Sched = Private.Sched

exception Boom

let error_of f =
  match f () with
  | _ ->
      "no error"
  | exception Net.Error (e, _) ->
      Format.asprintf "%a" Private.Op.pp_error e

(* Each function checks its range before the world, and the error has its name.
   Without the check, the world reads 0 bytes, or [Bytes] raises with its own
   name. *)
let bad_ranges () =
  Sim.run (fun sim ->
      let b = Bytes.create 4 in
      let c, _ = Sim.socket_pair sim in
      raises (Invalid_argument "Net.read_now") (fun () ->
          ignore (Net.read_now c b 0 0) ) ;
      raises (Invalid_argument "Net.write_now") (fun () ->
          ignore (Net.write_now c b 5 0) ) ;
      let u = Net.udp ("mut_io", 1) in
      raises (Invalid_argument "Net.sendto") (fun () ->
          Net.sendto u b 5 0 ("mut_io", 1) ) ;
      raises (Invalid_argument "Net.recvfrom") (fun () ->
          ignore (Net.recvfrom u b 0 0) ) ;
      let r, w = Pipe.create () in
      Pipe.write w "data" ;
      raises (Invalid_argument "Pipe.read") (fun () ->
          ignore (Pipe.read r b 0 0) ) ;
      raises (Invalid_argument "Pipe.write_sub") (fun () ->
          ignore (Pipe.write_sub w b 5 0) ) ;
      let f = File.openfile [Read; Write; Create] "/ranges" in
      File.write f "data" ;
      raises (Invalid_argument "File.read") (fun () ->
          ignore (File.read f b 0 0) ) ;
      raises (Invalid_argument "File.pread") (fun () ->
          ignore (File.pread f ~file_offset:0 b 0 0) ) ;
      raises (Invalid_argument "File.write") (fun () ->
          ignore (File.write_sub f b 5 0) ) ;
      raises (Invalid_argument "File.pwrite") (fun () ->
          ignore (File.pwrite f ~file_offset:0 b 5 0) ) ;
      File.close f )

let with_yield_bytes n f =
  let old = !Private.Net.yield_bytes in
  Private.Net.yield_bytes := n ;
  Fun.protect ~finally:(fun () -> Private.Net.yield_bytes := old) f

(* The switches of a ticker fiber while [reads] runs. The ticker runs only when
   the reader yields. *)
let ticks_during reads =
  let ticks = ref 0 in
  Fiber.scope (fun s ->
      Fiber.fork_daemon s (fun () ->
          while true do
            incr ticks ; Fiber.yield ()
          done ) ;
      Fiber.yield () ;
      let before = !ticks in
      reads () ; !ticks - before )

(* With a limit of 100 bytes, 100 reads of 10 bytes that always find data yield
   10 times, or 9 times if the reader yields one byte later. *)
let stream_reader_yields () =
  with_yield_bytes 100 (fun () ->
      Sim.run (fun sim ->
          let a, b = Sim.socket_pair sim in
          Net.write a (String.make 1000 'x') ;
          Time.sleep Mono (Span.ms 1) ;
          let buf = Bytes.create 10 in
          let n =
            ticks_during (fun () ->
                for _ = 1 to 100 do
                  equal int 10 (Net.read b buf 0 10)
                done )
          in
          at_least int ~than:9 n ; at_most int ~than:10 n ) )

(* An empty datagram counts as one byte: 50 empty datagrams with a limit of 10
   bytes yield 5 times, or 4 times if the reader yields one byte later. *)
let datagram_reader_yields () =
  with_yield_bytes 10 (fun () ->
      Sim.run (fun _ ->
          let u1 = Net.udp ("mut_io", 2) and u2 = Net.udp ("mut_io", 3) in
          let buf = Bytes.create 1 in
          for _ = 1 to 50 do
            Net.sendto u1 buf 0 0 ("mut_io", 3)
          done ;
          Time.sleep Mono (Span.ms 1) ;
          let n =
            ticks_during (fun () ->
                for _ = 1 to 50 do
                  equal int 0 (fst (Net.recvfrom u2 buf 0 1))
                done )
          in
          at_least int ~than:4 n ; at_most int ~than:5 n ) )

(* Outside a fiber, [close] and [reset] raise [Not_in_fiber] before they change
   the state, also for a closed socket. *)
let close_outside_fiber () =
  let a, b =
    Sim.run (fun sim ->
        let a, b = Sim.socket_pair sim in
        Net.close b ; (a, b) )
  in
  let outside f =
    raises_match (function Fiber.Not_in_fiber _ -> true | _ -> false) f
  in
  outside (fun () -> Net.close a) ;
  outside (fun () -> Net.close b) ;
  outside (fun () -> Net.reset a) ;
  outside (fun () -> Net.reset b) ;
  is_true (Atomic.get a.Sched.rc = Sched.Open 0)

(* A fiber in a cancelled context does not start a job: the directory does not
   come. *)
let cancelled_job () =
  Sim.run (fun _ ->
      let r =
        Fiber.scope (fun s ->
            Fiber.cancel s Boom ;
            match Dir.mkdir "/cancelled" with
            | () ->
                "no cancel"
            | exception Fiber.Cancelled Boom ->
                "cancelled" )
      in
      equal string "cancelled" r ;
      Time.sleep Mono (Span.ms 1) ;
      equal string "ENOENT" (error_of (fun () -> File.stat "/cancelled")) )

let tests =
  [ group "io checks"
      [ test
          "read_now, write_now, sendto, recvfrom, Pipe and File check the \
           range with their own name"
          bad_ranges
      ; test "a reader of a stream that always finds data yields"
          stream_reader_yields
      ; test "a reader of datagrams yields, and an empty one counts as one byte"
          datagram_reader_yields
      ; test
          "close and reset outside a fiber raise Not_in_fiber and change \
           nothing"
          close_outside_fiber
      ; test "a fiber in a cancelled context does not start a job" cancelled_job
      ] ]
