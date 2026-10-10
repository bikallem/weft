open Weft.Private

(* SplitMix64: a fixed PRNG, so that a seed gives the same run with each version
   of OCaml (design section 2.8). *)
module Prng = struct
  type t = {mutable s: int64}

  let make seed = {s= Int64.of_int seed}

  let next t =
    t.s <- Int64.add t.s 0x9E3779B97F4A7C15L ;
    let z = t.s in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 30))
        0xBF58476D1CE4E5B9L
    in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 27))
        0x94D049BB133111EBL
    in
    Int64.logxor z (Int64.shift_right_logical z 31)

  (* A value in the range 0 to [n - 1], with no bias. *)
  let int t n =
    if
      ( n <= 0 )
      [@mutate off "the scheduler asks only for n >= 2; Int64.rem fails at 0"]
    then invalid_arg "random" ;
    let n64 = Int64.of_int n in
    let rec draw () =
      let x = Int64.shift_right_logical (next t) 2 in
      let r = Int64.rem x n64 in
      if
        ( Int64.compare (Int64.sub x r) (Int64.sub (Int64.shift_left 1L 62) n64)
        > 0 )
        [@mutate off "the two limits differ in one draw of 2^62"]
      then draw ()
      else Int64.to_int r
    in
    draw ()

  let bytes t b =
    for i = 0 to Bytes.length b - 1 do
      Bytes.set b i (Char.chr (Int64.to_int (Int64.logand (next t) 0xffL)))
    done
end

exception Deadlock of string list

exception Lost_wakeup of string list

type t =
  { seed: int
  ; prng: Prng.t
  ; auto: bool
  ; mutable now: int64  (** The virtual [Mono] time, in nanoseconds. *)
  ; posix0: int64  (** [Posix] at [Mono] 0. *)
  ; mutable woken: bool
  ; mutable sched: Sched.t option }

let seed t = t.seed

let sched t = Option.get t.sched

let run ?(seed = 0) ?(auto = true) main =
  let t =
    { seed
    ; prng= Prng.make seed
    ; auto
    ; now= 0L
    ; posix0= 1_700_000_000_000_000_000L
    ; woken= false
    ; sched= None }
  in
  let open Effect.Deep in
  match_with
    (fun () ->
      Sched.run ~slack:Weft.Span.zero
        ~on_create:(fun s -> t.sched <- Some s)
        (fun () -> main t) )
    ()
    { retc= Fun.id
    ; exnc=
        (fun e ->
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) )
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | World.Now line ->
              Some
                (fun (k : (a, _) continuation) ->
                  let ns =
                    match line with
                    | Weft.Clock.Mono ->
                        t.now
                    | Weft.Clock.Posix ->
                        Int64.add t.posix0 t.now
                  in
                  continue k (Weft.Clock.of_ns line ns) )
          | World.Submit _ ->
              .
          | World.Random n ->
              Some (fun k -> continue k (Prng.int t.prng n))
          | World.Random_bytes b ->
              Some (fun k -> continue k (Prng.bytes t.prng b))
          | World.Switch _ ->
              Some (fun k -> continue k ())
          | World.Wake _ ->
              Some
                (fun k ->
                  t.woken <- true ;
                  continue k () )
          | World.Wait (_, w) ->
              Some
                (fun k ->
                  if t.woken then (
                    t.woken <- false ;
                    continue k [World.Ready.Woken] )
                  else
                    match w.timeout with
                    | World.Wait.Poll ->
                        continue k []
                    | World.Wait.Sleep -> (
                        let s = sched t in
                        ( match
                            Sched_debug.lost s
                              ~now:(Weft.Clock.of_ns Mono t.now)
                          with
                        | [] ->
                            ()
                        | l ->
                            raise (Lost_wakeup l) ) ;
                        match w.deadline with
                        | Some d when t.auto ->
                            let d = Weft.Clock.to_ns Mono d in
                            t.now <- Int64.max t.now d ;
                            continue k [World.Ready.Alarm]
                        | _ ->
                            raise (Deadlock (Sched_debug.blocked s)) ) )
          | _ ->
              None ) }
