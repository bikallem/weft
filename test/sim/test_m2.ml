(* The tests of the library layers on the simulator (design section 2.14,
   M2). *)

open Windtrap
open Weft
module Sim = Weft_sim

(* Each test of the fiber layer runs with 200 seeds on 4 virtual domains. *)
let seeds_200 =
  let n = ref 0 in
  { Portable.name= "200 seeds"
  ; domains= 4
  ; run=
      (fun f ->
        let rec go seed =
          let v = Sim.run ~seed ~domains:4 (fun _ -> f ()) in
          if seed = 200 then v else go (seed + 1)
        in
        go 1 )
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("m2", !n) in
        (Net.listen a, a) )
  ; tmp=
      (let n = ref 0 in
       fun () ->
         incr n ;
         let d = Printf.sprintf "/tmp%d" !n in
         Dir.mkdir d ; d ) }

module Sched = Private.Sched

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* With a time cost, each job takes 0 to 10 ms of virtual time. *)
let cost seed =
  Sim.run ~seed (fun sim ->
      Sim.set_blocking_cost sim (Span.ms 10) ;
      let t0 = Time.now Mono in
      Dir.mkdir "/d" ;
      let f = File.openfile [Write; Create] "/d/f" in
      File.write f "x" ; File.close f ; ms_since t0 )

(* A cancel during a job: the fiber detaches, and an fd that the job opened
   closes. A read that a cancel ends holds the fd until the job completes, so
   the close of the file waits for it (the simulator checks it). *)
let detach seed =
  Sim.run ~seed ~domains:4 (fun sim ->
      let f = File.openfile [Write; Create] "/f" in
      File.write f (String.make 100 'x') ;
      File.close f ;
      let before = Sim.open_fds sim in
      Sim.set_blocking_cost sim (Span.ms 10) ;
      let timeouts = ref 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 10 do
            Fiber.fork s (fun () ->
                match
                  Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                      let f = File.openfile [Read] "/f" in
                      Fun.protect
                        ~finally:(fun () -> File.close f)
                        (fun () -> File.read f (Bytes.create 10) 0 10) )
                with
                | n ->
                    equal int 10 n
                | exception Fiber.Timeout ->
                    incr timeouts )
          done ) ;
      (* The detached jobs complete. *)
      Time.sleep Mono (Span.ms 20) ;
      equal int before (Sim.open_fds sim) ;
      !timeouts )

(* A DNS server that never answers: [with_timeout] ends at the deadline. *)
let dns_timeout () =
  Sim.run (fun sim ->
      Sim.set_dns sim "slow.example" Sim.No_answer ;
      let t0 = Time.now Mono in
      raises Fiber.Timeout (fun () ->
          Fiber.with_timeout Mono (Span.s 2) (fun () ->
              Dns.resolve "slow.example" ) ) ;
      equal int 2000 (ms_since t0) )

let dns () =
  Sim.run (fun sim ->
      Sim.set_dns sim "server" (Sim.Addresses ["10.0.0.1"; "10.0.0.2"]) ;
      Sim.set_dns sim "alias" (Sim.Addresses ["10.0.0.2"]) ;
      equal (list string) ["10.0.0.1"; "10.0.0.2"] (Dns.resolve "server") ;
      equal string "alias" (Dns.reverse "10.0.0.2") ;
      equal string "server" (Dns.reverse "10.0.0.1") ;
      let err f =
        match f () with
        | _ ->
            "no error"
        | exception Dns.Error (e, _) ->
            Format.asprintf "%a" Private.Op.pp_error e
      in
      equal string "ENOENT" (err (fun () -> Dns.resolve "nobody")) ;
      equal string "ENOENT" (err (fun () -> Dns.reverse "10.9.9.9")) )

let jobs =
  group "jobs"
    [ test "a job takes a time cost from the seed" (fun () ->
          let costs = List.init 50 (fun s -> cost (s + 1)) in
          List.iter (fun c -> at_most int ~than:40 c) costs ;
          greater int ~than:1 (List.length (List.sort_uniq compare costs)) ;
          equal (list int) costs (List.init 50 (fun s -> cost (s + 1))) )
    ; test "with no cost, a job takes no time" (fun () ->
          Sim.run (fun _ ->
              let t0 = Time.now Mono in
              Dir.mkdir "/x" ;
              equal int 0 (ms_since t0) ) )
    ; test "a cancel detaches from the job, and no fd stays open" (fun () ->
          let total = ref 0 in
          for seed = 1 to 200 do
            total := !total + detach seed
          done ;
          (* Some jobs end in time, and some do not. *)
          greater int ~than:0 !total ;
          less int ~than:2000 !total )
    ; test "with_timeout around a DNS that never answers ends at the deadline"
        dns_timeout
    ; test "the name table of Dns" dns ]

let tests =
  [ Windtrap.group "200 seeds"
      [Portable.layers seeds_200; Portable.files seeds_200]
  ; jobs ]
