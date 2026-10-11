(* The tests of many domains on Linux (design section 2.14, M1). *)

open Windtrap
open Weft
module S = Private.Sched

let domains = 4

(* A new directory for each test that needs one. The directories go at the
   exit. *)
let tmp =
  let base = Filename.temp_dir "weft-test" "" and n = ref 0 in
  at_exit (fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote base))) ;
  fun () ->
    incr n ;
    let d = Filename.concat base (string_of_int !n) in
    Sys.mkdir d 0o755 ; d

let world =
  { Portable.name= "linux on 4 domains"
  ; domains
  ; run= (fun f -> Weft_linux.run ~domains f)
  ; listen=
      (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        (l, Net.Tcp ("127.0.0.1", Weft_linux.local_port l)) )
  ; tmp }

(* Timers on many domains end after their deadlines. *)
let timers () =
  Weft_linux.run ~domains (fun () ->
      Fiber.scope (fun s ->
          for i = 1 to 16 do
            Fiber.fork s (fun () ->
                let t0 = Time.now Mono in
                Time.sleep Mono (Span.ms (i mod 5)) ;
                at_least int64
                  ~than:(Int64.of_int (i mod 5 * 1_000_000))
                  (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )
          done ) )

let open_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* 2,000 connections on 4 domains, and no fd stays open. *)
let no_fd_leak () =
  let before = open_fds () in
  Weft_linux.run ~domains (fun () ->
      let l, a = world.listen () in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Portable.echo_server l) ;
          Fiber.scope (fun clients ->
              for _ = 1 to 8 do
                Fiber.fork clients (fun () ->
                    for _ = 1 to 250 do
                      let c = Net.connect a in
                      Net.write c "x" ;
                      equal string "x" (Portable.read_n c 1) ;
                      Net.close c
                    done )
              done ) ) ;
      Net.close l ) ;
  equal int before (open_fds ())

(* M3: F1 wakes F2 into its next slot, then computes 100 ms with no switch.
   Another domain takes F2 from the next slot. *)
let next_slot_steal () =
  Weft_linux.run ~domains (fun () ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          let got =
            Fiber.fork_promise s (fun () ->
                ignore (sync (Chan.recv c)) ;
                Time.now Mono )
          in
          Fiber.yield () ;
          let t0 = Time.now Mono in
          Fiber.fork s (fun () ->
              sync (Chan.send c ()) ;
              let rec spin () =
                if Span.to_ns (Time.diff Mono (Time.now Mono) t0) < 100_000_000L
                then spin ()
              in
              spin () ) ;
          less int64 ~than:50_000_000L
            (Span.to_ns (Time.diff Mono (Promise.await got) t0)) ) )

(* M3: a domain with no handler fills an Ivar from outside a fiber, while each
   domain of the run sleeps. *)
let outside_domain () =
  Weft_linux.run ~domains (fun () ->
      let iv = Ivar.create () in
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> sync (Ivar.read iv)) in
          let d =
            Domain.spawn (fun () ->
                Unix.sleepf 0.01 ;
                Ivar.fill_from_outside iv 42 )
          in
          equal int 42 (Promise.await p) ;
          is_true (Domain.join d) ) )

(* M3: pinned fibers run only on domain 0, also when other domains wake them. *)
let pinned () =
  Weft_linux.run ~domains (fun () ->
      let off = Atomic.make 0 and c = Chan.create () in
      let check () = if S.prog "test" S.Dom_index <> 0 then Atomic.incr off in
      Fiber.scope (fun s ->
          for _ = 1 to 20 do
            Fiber.fork ~pinned:true s (fun () ->
                for _ = 1 to 10 do
                  check () ; Fiber.yield ()
                done ;
                Time.sleep Mono (Span.ms 1) ;
                check () ;
                sync (Chan.recv c) ;
                check () )
          done ;
          for _ = 1 to 20 do
            Fiber.fork s (fun () ->
                Time.sleep Mono (Span.ms 2) ;
                sync (Chan.send c ()) )
          done ) ;
      equal int 0 (Atomic.get off) )

let () =
  exit
    (Windtrap.run "linux_md"
       ( Portable.tests world
       @ [ group "linux"
             [ test "timers on many domains end after their deadlines" timers
             ; test "a thief takes the next slot of a busy domain"
                 next_slot_steal
             ; test "a domain with no handler fills an Ivar" outside_domain
             ; test "pinned fibers run only on domain 0" pinned
             ; test "2,000 connections on 4 domains with no fd leak" no_fd_leak
             ] ] ) )
