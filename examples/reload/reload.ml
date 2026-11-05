(* A configuration that a program reads again at SIGHUP, and a worker that uses
   the new values at once. The code uses only the API of weft, so it runs in
   each world. *)

open Weft

type config = {period: Span.t; message: string}

(* The text of a configuration: lines "key=value", with the keys [period_ms] and
   [message]. *)
let parse text =
  let field key =
    String.split_on_char '\n' text
    |> List.find_map (fun line ->
        match String.index_opt line '=' with
        | Some i when String.trim (String.sub line 0 i) = key ->
            Some
              (String.trim
                 (String.sub line (i + 1) (String.length line - i - 1)) )
        | _ ->
            None )
  in
  match (field "period_ms", field "message") with
  | Some p, Some message -> (
    match int_of_string_opt p with
    | Some ms when ms > 0 ->
        Ok {period= Span.ms ms; message}
    | _ ->
        Error ("period_ms is not a positive number: " ^ p) )
  | _ ->
      Error "period_ms or message is missing"

(* All of the bytes of a file. Each read runs on the thread pool of the world,
   thus the domain does not block. *)
let read_file path =
  let f = File.openfile [Read] path in
  Fun.protect
    ~finally:(fun () -> File.close f)
    (fun () ->
      let b = Buffer.create 256 and chunk = Bytes.create 4096 in
      let rec loop () =
        match File.read f chunk 0 4096 with
        | 0 ->
            Buffer.contents b
        | n ->
            Buffer.add_subbytes b chunk 0 n ;
            loop ()
      in
      loop () )

let load path =
  match read_file path with
  | text ->
      parse text
  | exception (File.Error _ as e) ->
      Error (path ^ ": " ^ Printexc.to_string e)

(* Read the file again at each [signal], and set [config]. A file that is not
   correct goes to [on_error], and [config] keeps its value. The world must
   watch [signal]. *)
let watch ?(signal = Signal.SIGHUP) ~on_error path config =
  while true do
    ignore (sync (Signal.received signal)) ;
    match load path with Ok c -> Var.set config c | Error e -> on_error e
  done

(* Give the message of the configuration to [emit] at each period. The worker
   also waits for a change of the configuration. Thus a new period takes effect
   at once, not after the old period. The next tick comes one period after the
   last tick, or at once if that time is already past. *)
let ticker config ~emit =
  let next last c =
    match Time.add Mono last c.period with
    | Some t when Span.compare (Time.diff Mono t (Time.now Mono)) Span.zero > 0
      ->
        t
    | _ ->
        Time.now Mono
  in
  let rec loop seen last =
    let deadline = next last (Var.value seen) in
    match
      sync
        (choose
           [ wrap (Var.changed seen) (fun v -> `Changed v)
           ; wrap (Time.at Mono deadline) (fun () -> `Tick) ] )
    with
    | `Changed v ->
        loop v last
    | `Tick ->
        emit (Var.value seen).message ;
        loop seen deadline
  in
  loop (Var.view config) (Time.now Mono)
