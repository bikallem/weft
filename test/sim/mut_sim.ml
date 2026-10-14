(* The tests that kill the survivors of the mutation run in the simulator
   (design section 2.17): the file system, the jobs, the edges, the processes,
   the wall clock and the end of a run. *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched
module Op = Private.Op

let error_of = Portable.error_of

(* A world operation with no check of the core. *)
let submit op = Private.World.perform "test" (Private.World.Submit op)

let ok = function
  | Ok v ->
      v
  | Error e ->
      failwith (Format.asprintf "%a" Op.pp_error e)

(* The text of the error of a world operation, or "no error". *)
let op_error = function
  | Ok _ ->
      "no error"
  | Error e ->
      Format.asprintf "%a" Op.pp_error e

(* A job that completes at once, with no time cost. *)
let job j =
  let id = ok (submit (Op.Start (0, j))) in
  ok (submit (Op.Finish (id, j)))

(* The model error of [f], or "no model error". *)
let model_error f =
  match Sim.run f with
  | () ->
      "no model error"
  | exception Sim.Model_error m ->
      m

let files =
  group "files"
    [ test "an open with O_CREAT of a path that ends with / gives EISDIR"
        (fun () ->
          Sim.run (fun _ ->
              equal string "EISDIR"
                (error_of (fun () -> File.openfile [Write; Create] "/new/")) ;
              equal string "ENOENT" (error_of (fun () -> File.stat "/new")) ) )
    ; test "a file that grows keeps its data" (fun () ->
          Sim.run (fun _ ->
              let f = File.openfile [Read; Write; Create] "/f" in
              File.write f "abc" ;
              equal int 1
                (File.pwrite f ~file_offset:100 (Bytes.of_string "x") 0 1) ;
              let buf = Bytes.create 101 in
              equal int 101 (File.pread f ~file_offset:0 buf 0 101) ;
              equal string
                ("abc" ^ String.make 97 '\000' ^ "x")
                (Bytes.to_string buf) ;
              File.close f ) )
    ; test "a truncate past the end gives zeros" (fun () ->
          Sim.run (fun _ ->
              let f = File.openfile [Read; Write; Create] "/f" in
              File.write f "abc" ;
              File.truncate f 1000 ;
              equal int 1000 (File.fstat f).size ;
              let buf = Bytes.create 1000 in
              equal int 1000 (File.pread f ~file_offset:0 buf 0 1000) ;
              equal string
                ("abc" ^ String.make 997 '\000')
                (Bytes.to_string buf) ;
              File.close f ) )
    ; test "a rename of a directory to itself does nothing" (fun () ->
          Sim.run (fun _ ->
              Dir.mkdir "/d" ;
              let f = File.openfile [Write; Create] "/d/x" in
              File.close f ;
              Dir.rename "/d" "/d" ;
              equal (list string) ["x"] (Dir.readdir "/d") ) ) ]

let jobs =
  group "jobs"
    [ test "close(2) of an fd that a job uses gives Model_error" (fun () ->
          contains ~sub:"while job"
            (model_error (fun sim ->
                 let fd =
                   job (Op.Open ("/f", [O_read; O_write; O_create], 0))
                 in
                 Sim.set_blocking_cost sim (Span.ms 10) ;
                 ignore
                   (ok
                      (submit
                         (Op.Start (0, Op.Pread (fd, Bytes.create 4, 0, 4, 0))) ) ) ;
                 ignore (submit (Op.Close fd)) ) ) )
    ; test "a second Finish of a job gives Model_error" (fun () ->
          contains ~sub:"Finish of job"
            (model_error (fun _ ->
                 let j = Op.Stat "/" in
                 let id = ok (submit (Op.Start (0, j))) in
                 ignore (ok (submit (Op.Finish (id, j)))) ;
                 ignore (submit (Op.Finish (id, j))) ) ) ) ]

(* The edges of all domains. *)
let edges sim =
  Array.fold_left (fun n d -> n + d.S.stats.S.edges) 0 (Sim.sched sim).S.doms

let net =
  group "edges and names"
    [ test "a wait on an empty pipe gets no edge before the data" (fun () ->
          Sim.run (fun sim ->
              let r, w = Pipe.create () in
              let before = edges sim in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      equal int 1 (Pipe.read r (Bytes.create 1) 0 1) ) ;
                  Fiber.yield () ;
                  equal int 0 (edges sim - before) ;
                  Pipe.write w "x" ) ;
              equal int 1 (edges sim - before) ;
              Pipe.close r ;
              Pipe.close w ) )
    ; test "extra_edges gives edges with no change" (fun () ->
          let idle = ref 0 in
          for seed = 1 to 10 do
            Sim.run ~seed (fun sim ->
                Sim.net_faults sim {Sim.no_faults with extra_edges= true} ;
                let r, w = Pipe.create () in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Pipe.read r (Bytes.create 1) 0 1) ) ;
                    for _ = 1 to 50 do
                      Time.sleep Mono (Span.ms 1)
                    done ;
                    Pipe.write w "x" ) ;
                idle :=
                  !idle
                  + Array.fold_left
                      (fun n d -> n + d.S.stats.S.idle_edges)
                      0 (Sim.sched sim).S.doms ;
                Pipe.close r ;
                Pipe.close w )
          done ;
          greater int ~than:0 !idle )
    ; test "a listen after the close of a listener on the same address"
        (fun () ->
          Sim.run (fun sim ->
              let a = Sim.addr sim "again:1" in
              Net.close (Net.listen a) ;
              Net.close (Net.listen a) ) ) ]

let processes =
  group "processes"
    [ test "a wait of a child that the parent reaped gives ECHILD" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "p" (fun _ ->
                  Sim.program ~runs:(Span.ms 1) () ) ;
              let r =
                ok
                  (submit
                     (Op.Spawn
                        { prog= "p"
                        ; argv= ["p"]
                        ; env= None
                        ; cwd= None
                        ; stdin= Inherit
                        ; stdout= Inherit
                        ; stderr= Inherit } ) )
              in
              Time.sleep Mono (Span.ms 2) ;
              is_true (ok (submit (Op.Wait_child r.pidfd)) = Some (Exited 0)) ;
              equal string "ECHILD" (op_error (submit (Op.Wait_child r.pidfd))) ;
              ok (submit (Op.Close r.pidfd)) ) ) ]

(* A step of the wall clock wakes a domain that sleeps until a Posix deadline.
   The other domain does not let the program become quiescent. It gives true if
   the waiter slept on domain 1. *)
let posix_step seed =
  Sim.run ~seed ~domains:2 ~livelock:100_000 (fun sim ->
      let fired = Atomic.make false and on = ref (-1) in
      let at = Option.get (Time.add Posix (Time.now Posix) (Span.s 10)) in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              on := S.prog "test" S.Dom_index ;
              sync (Time.at Posix at) ;
              Atomic.set fired true ) ;
          Fiber.fork ~pinned:true s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              Sim.set_posix sim (Option.get (Time.add Posix at (Span.s 1))) ;
              while not (Atomic.get fired) do
                Fiber.yield ()
              done ) ) ;
      !on = 1 )

let clock =
  group "wall clock"
    [ test "a step of the wall clock wakes a sleeping domain, with 20 seeds"
        (fun () ->
          is_true
            (List.exists Fun.id (List.init 20 (fun s -> posix_step (s + 1)))) )
    ]

(* The roots of [hold]. *)
let roots = Array.make 4 None

(* [hold n v] keeps [v] until a chain of [n] finalisers runs. Each finaliser
   removes the root of the value of the next one. In OCaml 5.5, a full major GC
   does 3 cycles, and each cycle runs one finaliser of the chain. Thus [v]
   becomes free in the second full major GC. *)
let[@inline never] hold n v =
  let v = ref (Obj.repr v) in
  for i = 0 to n - 1 do
    roots.(i) <- Some !v ;
    let h = ref i in
    Gc.finalise (fun _ -> roots.(i) <- None) h ;
    v := Obj.repr h
  done

let run_end =
  group "the end of a run"
    [ test "after a run, no run takes a fill from outside" (fun () ->
          Sim.run ~outside_fills:true (fun _ -> ()) ;
          is_false (Ivar.fill_from_outside (Ivar.create ()) ()) )
    ; test "a promise behind a chain of 4 finalisers gives Unawaited" (fun () ->
          raises (Sim.Unawaited ["Not_found"]) (fun () ->
              Sim.run (fun _ ->
                  Fiber.scope (fun s ->
                      hold 4 (Fiber.fork_promise s (fun () -> raise Not_found)) ) ) ) )
    ]

let tests = [group "mutants" [files; jobs; net; processes; clock; run_end]]
