(* A supervisor: it runs a program, and starts it again when it fails. The code
   uses only the API of weft, so it runs in each world. *)

open Weft

type event =
  | Started of int  (** The pid of the child. *)
  | Output of string  (** A line of the stdout of the child. *)
  | Exited of Process.status
  | Restart_in of Span.t
  | Stopped

(* Give each line of [pipe] to [log], until the end of the stream. *)
let copy_lines pipe log =
  let buf = Bytes.create 4096 and pending = Buffer.create 128 in
  let flush_lines () =
    let s = Buffer.contents pending in
    let parts = String.split_on_char '\n' s in
    let rec go = function
      | [last] ->
          Buffer.clear pending ;
          Buffer.add_string pending last
      | line :: rest ->
          log (Output line) ; go rest
      | [] ->
          ()
    in
    go parts
  in
  let rec loop () =
    match Pipe.read pipe buf 0 (Bytes.length buf) with
    | 0 ->
        if Buffer.length pending > 0 then log (Output (Buffer.contents pending))
    | n ->
        Buffer.add_subbytes pending buf 0 n ;
        flush_lines () ;
        loop ()
  in
  loop ()

(* Stop the child: SIGTERM, then SIGKILL if it does not exit in [grace]. *)
let terminate ~grace p =
  Process.kill p SIGTERM ;
  match Fiber.with_timeout Mono grace (fun () -> Process.wait p) with
  | status ->
      status
  | exception Fiber.Timeout ->
      Process.kill p SIGKILL ; Process.wait p

(* One run of the child. Its stdout goes to [log] in a fiber of its own. The
   supervisor waits for the first of two events: the exit of the child, or
   [stop]. The scope waits for the copy of the output, also after a stop.

   If a cancel from outside stops the supervisor, for example a timeout, the
   child must not stay alive. Thus the handler stops the child in a shield, and
   then raises the cancel again. *)
let run_once ~stop ~grace ~log prog args =
  let p = Process.spawn ~stdout:Piped prog args in
  log (Started (Process.pid p)) ;
  let out = Option.get (Process.stdout p) in
  Fun.protect
    ~finally:(fun () -> Pipe.close out)
    (fun () ->
      try
        Fiber.scope (fun s ->
            Fiber.fork s (fun () -> copy_lines out log) ;
            sync
              (choose
                 [ wrap (Process.exited p) (fun st -> `Exited st)
                 ; wrap stop (fun () -> `Stop (terminate ~grace p)) ] ) )
      with Fiber.Cancelled _ as e ->
        let bt = Printexc.get_raw_backtrace () in
        log (Exited (Fiber.shield (fun () -> terminate ~grace p))) ;
        Printexc.raise_with_backtrace e bt )

(* Run [prog] until it exits with the code 0, or until [stop] is ready. After a
   failure, wait, then start it again. The wait starts at [min_backoff], and it
   doubles after each failure, up to [max_backoff]. A run that is longer than
   [healthy] sets the wait to [min_backoff] again. *)
let supervise ?(stop = never) ?(min_backoff = Span.ms 100)
    ?(max_backoff = Span.s 10) ?(healthy = Span.s 5) ?(grace = Span.s 5) ~log
    prog args =
  let double b =
    let d = Span.of_ns (Int64.mul 2L (Span.to_ns b)) in
    if Span.compare d max_backoff > 0 then max_backoff else d
  in
  let rec loop backoff =
    let t0 = Time.now Mono in
    match run_once ~stop ~grace ~log prog args with
    | `Stop status ->
        log (Exited status) ; log Stopped
    | `Exited (Process.Exited 0 as status) ->
        log (Exited status)
    | `Exited status -> (
        log (Exited status) ;
        let ran = Time.diff Mono (Time.now Mono) t0 in
        let backoff =
          if Span.compare ran healthy >= 0 then min_backoff else backoff
        in
        log (Restart_in backoff) ;
        match
          sync
            (choose
               [ wrap (Time.after Mono backoff) (fun () -> `Restart)
               ; wrap stop (fun () -> `Stop) ] )
        with
        | `Restart ->
            loop (double backoff)
        | `Stop ->
            log Stopped )
  in
  loop min_backoff
