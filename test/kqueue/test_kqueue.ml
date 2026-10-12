(* The tests of the kqueue world (design section 2.7): the portable tests on 1
   and on 4 domains, and the tests of the timers, the signals and the child
   processes. *)

open Windtrap
open Weft

(* A new directory for each test that needs one. The directories go at the
   exit. *)
let tmp =
  let base = Filename.temp_dir "weft-test" "" and n = ref 0 in
  at_exit (fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote base))) ;
  fun () ->
    incr n ;
    let d = Filename.concat base (string_of_int !n) in
    Sys.mkdir d 0o755 ; d

let world domains =
  { Portable.name= Printf.sprintf "kqueue on %d domains" domains
  ; domains
  ; run= (fun f -> Weft_kqueue.run ~domains f)
  ; listen=
      (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        (l, Net.Tcp ("127.0.0.1", Weft_kqueue.local_port l)) )
  ; udp=
      (fun () ->
        let s = Net.udp ("127.0.0.1", 0) in
        (s, ("127.0.0.1", Weft_kqueue.local_port s)) )
  ; tmp }

let elapsed_ns f =
  Weft_kqueue.run ~domains:1 (fun () ->
      let t0 = Time.now Mono in
      f () ;
      Int64.to_int (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )

(* A sleep never ends before its deadline. The upper limit is large: the CI
   machines are shared. *)
let sleep_10ms () =
  for _ = 1 to 20 do
    let ns = elapsed_ns (fun () -> Time.sleep Mono (Span.ms 10)) in
    at_least int ~than:10_000_000 ns ;
    less int ~than:50_000_000 ns
  done

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

(* A shell starts a background job with SIGINT ignored, and a child inherits
   this action. Thus the test gives SIGINT its default action for the time of
   the spawn. *)
let with_default_sigint f =
  let old = Sys.signal Sys.sigint Sys.Signal_default in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigint old) f

let child_kill () =
  with_default_sigint (fun () ->
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

let () =
  exit
    (Windtrap.run "kqueue"
       ( Portable.tests (world 1)
       @ Portable.tests (world 4)
       @ [ group "kqueue"
             [ test "a 10 ms sleep ends after 10 ms" sleep_10ms
             ; test "the pipes and the exit status of a child" child_output
             ; test "a child that exits at once" child_fast_exit
             ; test "a child stops at SIGINT" child_kill
             ; test "SIGTERM to the process is an event" sigterm
             ; test "run restores the action of a signal" signal_restored ] ] ) )
