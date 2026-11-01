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
             ; test "Weft_os is the Windows world" weft_os ] ] ) )
