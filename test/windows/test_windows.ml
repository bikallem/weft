(* The tests of the Windows world (design section 2.7, Windows): the portable
   tests on 1 and on 4 domains, and the tests of the timers, the children, the
   events of the console and the watchdog. *)

open Windtrap
open Weft

(* A call of the handler of the console, as Windows does: 0 is Ctrl-C, 1 is
   Ctrl-Break and 2 the close of the console. [true] if the world took the
   event. *)
external console_event : int -> bool = "weft_win_test_ctrl"

let world domains =
  Portable.loopback_world
    ~name:(Printf.sprintf "windows on %d domains" domains)
    ~domains
    {Portable.call= (fun f -> Weft_windows.run ~domains f)}

let read_pipe = Portable.drain Pipe.read

let output ?stdin ?cwd ?env prog args =
  let p = Process.spawn ?stdin ?cwd ?env ~stdout:Piped ~stderr:Null prog args in
  let out = Option.get (Process.stdout p) in
  let s = read_pipe out in
  Pipe.close out ;
  (s, Process.wait p)

let child_output () =
  Weft_windows.run ~domains:1 (fun () ->
      let s, st = output "cmd" ["/c"; "echo hi& exit 3"] in
      equal string "hi\r\n" s ;
      is_true (st = Process.Exited 3) ;
      (* The quotes of an argument with a space. *)
      let s, _ = output "cmd" ["/c"; "echo"; "a b"] in
      equal string "\"a b\"\r\n" s ;
      let s, _ = output ~cwd:"C:\\Windows" "cmd" ["/c"; "cd"] in
      equal string "C:\\Windows\r\n" s ;
      let s, _ =
        output
          ~env:["WEFT_X=42"; "SystemRoot=" ^ Sys.getenv "SystemRoot"]
          "cmd" ["/c"; "echo %WEFT_X%"]
      in
      equal string "42\r\n" s ;
      match Process.spawn "no-such-program-weft" [] with
      | _ ->
          failwith "no error"
      | exception Process.Error (Net.No_entry, "Process.spawn") ->
          () )

(* A pipe in each direction: sort reads all of its input. *)
let child_pipes () =
  Weft_windows.run ~domains:2 (fun () ->
      let p = Process.spawn ~stdin:Piped ~stdout:Piped "sort" [] in
      let i = Option.get (Process.stdin p)
      and o = Option.get (Process.stdout p) in
      Pipe.write i "b\r\na\r\n" ;
      Pipe.close i ;
      equal string "a\r\nb\r\n" (read_pipe o) ;
      Pipe.close o ;
      is_true (Process.wait p = Process.Exited 0) )

(* A child that exits at once: its exit can come before a fiber waits. *)
let child_fast_exit () =
  Weft_windows.run ~domains:2 (fun () ->
      for _ = 1 to 20 do
        let p = Process.spawn ~stdout:Null "cmd" ["/c"; "exit 0"] in
        is_true (Process.wait p = Process.Exited 0)
      done )

(* Each signal ends a child with the exit code 1. *)
let child_kill () =
  Weft_windows.run ~domains:1 (fun () ->
      let p = Process.spawn ~stdout:Null "ping" ["-n"; "30"; "127.0.0.1"] in
      Time.sleep Mono (Span.ms 20) ;
      Process.kill p SIGINT ;
      is_true (Process.wait p = Process.Exited 1) )

(* Ctrl-C and Ctrl-Break come as signals to a fiber that waits. After the
   unwatch, the world does not take the event: its default action ends the
   process. *)
let console_events () =
  let n =
    Weft_windows.run ~domains:2 ~signals:[SIGINT; Other_signal 21] (fun () ->
        Fiber.scope (fun s ->
            let got =
              Fiber.fork_promise s (fun () -> sync (Signal.received SIGINT))
            in
            Time.sleep Mono (Span.ms 10) ;
            is_true (console_event 0) ;
            let n = Promise.await got in
            is_true (console_event 1) ;
            at_least int ~than:1 (sync (Signal.received (Other_signal 21))) ;
            Signal.unwatch SIGINT ;
            is_false (console_event 0) ;
            n ) )
  in
  at_least int ~than:1 n ;
  (* After the run, the world takes no event. *)
  is_false (console_event 0)

(* A loopback connection: the client and the accepted socket. *)
let pair () =
  let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
  let c = Net.connect (Net.local_addr l) in
  let s, _ = Net.accept l in
  Net.close l ; (c, s)

(* The review of the Windows world: the I/O of a domain does not wait for
   another domain. A pinned fiber computes on domain 0. A fiber on domain 1
   reads a socket that another fiber writes, and it gets the data while domain 0
   computes. *)
let busy_domain_0 () =
  Weft_windows.run ~domains:2 ~watchdog:None (fun () ->
      let c, s = pair () in
      let spinning = Atomic.make false and got = Atomic.make false in
      Fiber.scope (fun sc ->
          let during =
            Fiber.fork_promise ~pinned:true sc (fun () ->
                Atomic.set spinning true ;
                let t0 = Unix.gettimeofday () in
                while Unix.gettimeofday () -. t0 < 2. && not (Atomic.get got) do
                  ()
                done ;
                Atomic.get got )
          in
          while not (Atomic.get spinning) do
            Fiber.yield ()
          done ;
          Fiber.fork sc (fun () ->
              Time.sleep Mono (Span.ms 20) ;
              Net.write c "x" ) ;
          let buf = Bytes.create 1 in
          equal int 1 (Net.read s buf 0 1) ;
          Atomic.set got true ;
          is_true (Promise.await during) ) ;
      Net.close c ;
      Net.close s )

(* The review: a second world, made and closed during a run, does not take the
   I/O of the first world. *)
let second_world () =
  Weft_windows.run ~domains:1 (fun () ->
      Weft_windows.close (Weft_windows.create ()) ;
      let c, s = pair () in
      Fiber.scope (fun sc ->
          Fiber.fork sc (fun () ->
              Time.sleep Mono (Span.ms 20) ;
              Net.write c "x" ) ;
          let buf = Bytes.create 1 in
          equal int 1
            (Fiber.with_timeout Mono (Span.s 5) (fun () -> Net.read s buf 0 1)) ) ;
      Net.close c ;
      Net.close s )

(* The review: at the end of a run, a write to a child that does not read stops,
   and the close of the world does not wait for it. *)
let pending_write_at_close () =
  let t0 = Unix.gettimeofday () in
  Weft_windows.run ~domains:1 (fun () ->
      let p =
        Process.spawn ~stdin:Piped ~stdout:Null "ping" ["-n"; "5"; "127.0.0.1"]
      in
      let i = Option.get (Process.stdin p) in
      Pipe.write i (String.make 100_000 'x') ;
      Pipe.close i ) ;
  less (float 0.001) ~than:3. (Unix.gettimeofday () -. t0)

let one_signal_world () =
  Weft_windows.run ~domains:1 ~signals:[SIGINT] (fun () ->
      raises
        (Invalid_argument "Weft_windows.create: another world watches signals")
        (fun () -> Weft_windows.create ~signals:[SIGINT] () ) ) ;
  Weft_windows.close (Weft_windows.create ~signals:[SIGINT] ())

(* Weft_os is the Windows world: its watchdog reports a fiber that computes with
   no yield. *)
let weft_os () =
  let before = Weft_windows.watchdog_reports () in
  Weft.run ~domains:1
    ~watchdog:(Some (Span.ms 10))
    (module Weft_os)
    (fun () ->
      let t0 = Sys.time () in
      while
        Weft_windows.watchdog_reports () = before && Sys.time () -. t0 < 2.
      do
        ()
      done ) ;
  equal int (before + 1) (Weft_windows.watchdog_reports ())

let () =
  exit
    (Windtrap.run "windows"
       ( Portable.tests (world 1)
       @ Portable.tests (world 4)
       @ [ group "windows"
             [ test "a 10 ms sleep ends after 10 ms"
                 (Portable.sleep_10ms ~median:20_000_000 (world 1))
             ; test "the output and the exit code of a child" child_output
             ; test "a pipe to a child and a pipe from it" child_pipes
             ; test "a child that exits at once" child_fast_exit
             ; test "Process.kill ends a child with the exit code 1" child_kill
             ; test "Ctrl-C and Ctrl-Break come as signals" console_events
             ; test "only one world at a time watches signals" one_signal_world
             ; test "a busy domain 0 does not stop the I/O of domain 1"
                 busy_domain_0
             ; test "a second world does not take the I/O of the first"
                 second_world
             ; test "the close of a world stops a write to a child"
                 pending_write_at_close
             ; test "Weft_os is the Windows world" weft_os ] ] ) )
