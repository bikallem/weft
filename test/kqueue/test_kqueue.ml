(* The tests of the kqueue world (design section 2.7): the portable tests on 1
   and on 4 domains, and the tests of the timers, the signals and the child
   processes. *)

open Windtrap
open Weft

let world domains =
  Portable.loopback_world
    ~name:(Printf.sprintf "kqueue on %d domains" domains)
    ~domains
    {Portable.call= (fun f -> Weft_kqueue.run ~domains f)}

let read_pipe = Portable.drain Pipe.read

let child_output () =
  Weft_kqueue.run ~domains:1 (fun () ->
      let p =
        Process.spawn ~stdout:Piped ~stderr:Null "sh"
          ["-c"; "echo hi; echo err >&2; exit 3"]
      in
      let out = Option.get (Process.stdout p) in
      equal string "hi\n" (read_pipe out) ;
      Pipe.close out ;
      is_true (Process.wait p = Process.Exited 3) ;
      let p = Process.spawn ~stdin:Piped ~stdout:Piped ~cwd:"/" "cat" [] in
      let i = Option.get (Process.stdin p)
      and o = Option.get (Process.stdout p) in
      Pipe.write i "through cat" ;
      Pipe.close i ;
      equal string "through cat" (read_pipe o) ;
      Pipe.close o ;
      is_true (Process.wait p = Process.Exited 0) ;
      match Process.spawn "no-such-program-weft" [] with
      | _ ->
          failwith "no error"
      | exception Process.Error (Net.No_entry, "Process.spawn") ->
          () )

(* A child that exits at once: its exit can come before the world watches it. *)
let child_fast_exit () =
  Weft_kqueue.run ~domains:2 (fun () ->
      for _ = 1 to 50 do
        let p = Process.spawn "true" [] in
        is_true (Process.wait p = Process.Exited 0)
      done )

let child_kill () =
  Portable.with_default_sigint (fun () ->
      Weft_kqueue.run ~domains:1 (fun () ->
          let p = Process.spawn "sleep" ["10"] in
          Time.sleep Mono (Span.ms 20) ;
          Process.kill p SIGINT ;
          is_true (Process.wait p = Process.Signaled SIGINT) ) )

(* SIGTERM to the process: a fiber that waits gets it, and run ends. *)
let sigterm () =
  let n =
    Weft_kqueue.run ~domains:1 ~signals:[SIGTERM] (fun () ->
        Fiber.scope (fun s ->
            let got =
              Fiber.fork_promise s (fun () -> sync (Signal.received SIGTERM))
            in
            Time.sleep Mono (Span.ms 10) ;
            Unix.kill (Unix.getpid ()) Sys.sigterm ;
            Promise.await got ) )
  in
  at_least int ~than:1 n

(* After run, the action of SIGTERM is the action before run. *)
let signal_restored () =
  Weft_kqueue.run ~domains:1 ~signals:[SIGUSR1] ignore ;
  is_true (Sys.signal Sys.sigusr1 Sys.Signal_default = Sys.Signal_default)

(* In the process of the test: the waiter raises, a later received and a second
   unwatch raise, and SIGTERM stays an event. After the unwatch, SIGUSR2 has its
   old action, and the end of the run does not change it. *)
let unwatch_waiter () =
  let not_watched = "Signal.received: the world does not watch this signal" in
  let old = Sys.signal Sys.sigusr2 Sys.Signal_ignore in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigusr2 old)
    (fun () ->
      Weft_kqueue.run ~domains:2 ~signals:[SIGUSR2; SIGTERM] (fun () ->
          Fiber.scope (fun s ->
              let p =
                Fiber.fork_promise s (fun () ->
                    match sync (Signal.received SIGUSR2) with
                    | _ ->
                        "no error"
                    | exception Invalid_argument m ->
                        m )
              in
              Time.sleep Mono (Span.ms 10) ;
              Signal.unwatch SIGUSR2 ;
              equal string not_watched (Promise.await p) ) ;
          raises (Invalid_argument not_watched) (fun () ->
              sync (Signal.received SIGUSR2) ) ;
          raises
            (Invalid_argument
               "Signal.unwatch: the world does not watch this signal" )
            (fun () -> Signal.unwatch SIGUSR2 ) ;
          (* The old action ignores it. *)
          Unix.kill (Unix.getpid ()) Sys.sigusr2 ;
          Unix.kill (Unix.getpid ()) Sys.sigterm ;
          at_least int ~than:1 (sync (Signal.received SIGTERM)) ;
          Sys.set_signal Sys.sigusr2 Sys.Signal_default ) ;
      is_true (Sys.signal Sys.sigusr2 Sys.Signal_ignore = Sys.Signal_default) )

(* A storm of signals while fibers run on small stacks (issue 6). The handler of
   the world must run on the signal stack (SA_ONSTACK): on a stack of a fiber,
   it corrupts the stack and the heap. A child shell sends SIGUSR1 in a loop for
   2 s, and 8 fibers on 2 domains run effect handlers. Then the test counts the
   signals of the storm. A copy of this test on Linux, with the handler with no
   SA_ONSTACK, crashed in each run. *)
type _ Effect.t += Step : int -> int Effect.t

let rec items n acc = if n = 0 then acc else items (n - 1) (n :: acc)

let small_fiber i =
  Effect.Deep.match_with
    (fun () ->
      let x = ref 0 in
      for j = 1 to 20 do
        x := !x + Effect.perform (Step j) + List.length (items 50 [])
      done ;
      !x + i )
    ()
    { retc= Fun.id
    ; exnc= raise
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Step j ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k j )
          | _ ->
              None ) }

let signal_storm () =
  let got =
    Weft_kqueue.run ~domains:2 ~watchdog:None ~signals:[SIGUSR1] (fun () ->
        let pid = string_of_int (Unix.getpid ()) in
        let sender =
          Process.spawn ~stderr:Null "sh"
            ["-c"; "while kill -USR1 " ^ pid ^ "; do :; done"]
        in
        let t0 = Time.now Mono in
        let late () =
          Int64.compare
            (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
            2_000_000_000L
          >= 0
        in
        let got = Atomic.make 0 in
        Fiber.scope (fun s ->
            Fiber.fork s (fun () ->
                while not (late ()) do
                  sync
                    (choose
                       [ wrap (Signal.received SIGUSR1) (fun n ->
                             ignore (Atomic.fetch_and_add got n) )
                       ; Time.after Mono (Span.ms 10) ] )
                done ) ;
            for _ = 1 to 8 do
              Fiber.fork s (fun () ->
                  while not (late ()) do
                    for i = 1 to 100 do
                      if small_fiber i <> 1210 + i then failwith "a bad value"
                    done ;
                    Fiber.yield ()
                  done )
            done ) ;
        Process.kill sender SIGKILL ;
        ignore (Process.wait sender) ;
        (* Domain 0 takes the counts only when it polls, and a busy domain polls
           each 61 switches. On a slow runner, domain 0 can make fewer switches
           in the 2 s, and the fiber then gets no signal. Thus take the counts
           of the storm that remain, until no signal comes in 100 ms. *)
        let rec rest () =
          match
            sync
              (choose
                 [ Signal.received SIGUSR1
                 ; wrap (Time.after Mono (Span.ms 100)) (fun () -> 0) ] )
          with
          | 0 ->
              ()
          | n ->
              ignore (Atomic.fetch_and_add got n) ;
              rest ()
        in
        rest () ; Atomic.get got )
  in
  at_least int ~than:100 got

(* Only one world at a time can watch signals: the handler is for the process.
   For a world with no signals, the handler is not necessary. A refused create
   does not release the handler of the open world, so a second try also
   raises. *)
let one_signal_world () =
  let another =
    Invalid_argument "Weft_kqueue.create: another world watches signals"
  in
  Weft_kqueue.run ~domains:1 ~signals:[SIGUSR1] (fun () ->
      raises another (fun () ->
          ignore (Weft_kqueue.create ~signals:[SIGUSR2] ()) ) ;
      raises another (fun () ->
          ignore (Weft_kqueue.create ~signals:[SIGUSR2] ()) ) ;
      Weft_kqueue.close (Weft_kqueue.create ()) ) ;
  (* After the close, a new world can watch signals. *)
  Weft_kqueue.close (Weft_kqueue.create ~signals:[SIGUSR2] ()) ;
  (* A second close of a world does nothing: it does not release the handler of
     a later world. *)
  let a = Weft_kqueue.create ~signals:[SIGUSR1] () in
  Weft_kqueue.close a ;
  let b = Weft_kqueue.create ~signals:[SIGUSR2] () in
  Weft_kqueue.close a ;
  raises another (fun () -> ignore (Weft_kqueue.create ~signals:[SIGUSR1] ())) ;
  Weft_kqueue.close b ;
  (* A number of domains out of the range raises before the claim. *)
  List.iter
    (fun domains ->
      raises
        (Invalid_argument
           "Weft_kqueue.create: the number of domains is not in the range 1 to \
            62" ) (fun () ->
          ignore (Weft_kqueue.create ~domains ~signals:[SIGUSR1] ()) ) )
    [0; 63] ;
  Weft_kqueue.close (Weft_kqueue.create ~signals:[SIGUSR1] ())

(* The watchdog parks while all domains sleep, as in test/linux (design section
   3.12, The park of the watchdog). With no park, an idle run of 200 ms gives
   about 40 checks. *)
let watchdog_park () =
  let checks =
    Weft_kqueue.run ~domains:4 (fun () ->
        Time.sleep Mono (Span.ms 20) ;
        let before = Weft_kqueue.watchdog_checks () in
        Time.sleep Mono (Span.ms 200) ;
        Weft_kqueue.watchdog_checks () - before )
  in
  at_most int ~than:10 checks

(* After a park, the watchdog reports a fiber that computes for 3 times the
   limit with no yield, on 1 and on 2 domains, as in test/linux. *)
let watchdog_after_park () =
  let busy_ms ms =
    let t0 = Unix.gettimeofday () in
    while Unix.gettimeofday () -. t0 < Float.of_int ms /. 1000. do
      ()
    done
  in
  List.iter
    (fun domains ->
      let before = Weft_kqueue.watchdog_reports () in
      Weft_kqueue.run ~domains
        ~watchdog:(Some (Span.ms 20))
        (fun () ->
          Time.sleep Mono (Span.ms 100) ;
          busy_ms 60 ) ;
      equal int (before + 1) (Weft_kqueue.watchdog_reports ()) )
    [1; 2]

let () =
  exit
    (Windtrap.run "kqueue"
       ( Portable.tests (world 1)
       @ Portable.tests (world 4)
       @ [ group "kqueue"
             [ test "a 10 ms sleep ends after 10 ms"
                 (Portable.sleep_10ms ~median:20_000_000 (world 1))
             ; test "the pipes and the exit status of a child" child_output
             ; test "a child that exits at once" child_fast_exit
             ; test "a child stops at SIGINT" child_kill
             ; test
                 "a child gets the default action for a signal that the parent \
                  ignores"
                 (Portable.child_signal_actions (world 1))
             ; test "SIGTERM to the process is an event" sigterm
             ; test "run restores the action of a signal" signal_restored
             ; test ~timeout:60. "after Signal.unwatch, SIGINT ends the process"
                 (Portable.unwatch_modes (world 1))
             ; test ~timeout:30.
                 "after Signal.unwatch, SIGINT ends a busy process"
                 (Portable.unwatch_busy (world 1))
             ; test "a waiter at Signal.unwatch raises" unwatch_waiter
             ; test ~timeout:60.
                 "a storm of signals while fibers use small stacks" signal_storm
             ; test "only one world at a time watches signals" one_signal_world
             ; test "the watchdog parks while all domains sleep" watchdog_park
             ; test "after a park, the watchdog reports a stall"
                 watchdog_after_park ] ] ) )
