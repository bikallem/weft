(* The measurement of the GC pauses of open question 6 (design section 2.13), on
   the Linux world. An echo server runs on 1, 2, 4 and 8 domains, and
   [Runtime_events] records the GC phases of each domain. Each run must fit in
   the ring of each domain, thus: OCAMLRUNPARAM=e=20 dune exec bench/q6.exe.
   With e=22, the reader of OCaml 5.5 gave "corrupt stream". *)

open Weft
module R = Runtime_events

(* The spans of the phases of the GC that stop the fibers of a domain. *)
let phases = R.[EV_MINOR; EV_MAJOR_SLICE; EV_STW_LEADER; EV_STW_HANDLER]

type spans =
  { open_: (int * R.runtime_phase, int64) Hashtbl.t
  ; spans: (R.runtime_phase, int64 list) Hashtbl.t
  ; mutable lost: int
  ; mutable window: int64 * int64
        (** The spans out of this window are not counted. *) }

let callbacks s =
  let runtime_begin ring ts phase =
    if List.mem phase phases then
      Hashtbl.replace s.open_ (ring, phase) (R.Timestamp.to_int64 ts)
  in
  let runtime_end ring ts phase =
    match Hashtbl.find_opt s.open_ (ring, phase) with
    | Some t0 ->
        Hashtbl.remove s.open_ (ring, phase) ;
        let t1 = R.Timestamp.to_int64 ts in
        let w0, w1 = s.window in
        if t0 >= w0 && t1 <= w1 then
          let l = Option.value ~default:[] (Hashtbl.find_opt s.spans phase) in
          Hashtbl.replace s.spans phase (Int64.sub t1 t0 :: l)
    | None ->
        ()
  in
  let lost_events _ n = s.lost <- s.lost + n in
  R.Callbacks.create ~runtime_begin ~runtime_end ~lost_events ()

let percentile p a =
  if Array.length a = 0 then 0L
  else
    a.(Int.min (Array.length a - 1) (int_of_float (p *. float (Array.length a))))

let sorted l =
  let a = Array.of_list l in
  Array.sort compare a ; a

let us ns = Int64.to_float ns /. 1000.

(* The live heap of the load with allocation: about 40 MB. *)
let live = ref [||]

(* C clients each do R round trips of 64 bytes. With [~alloc], the server
   allocates about 2 KB for each request, and it puts one string in the live
   heap. It gives the trips per second, the latency of each trip in ns, and the
   window of the traffic in the time of [Runtime_events]. *)
let echo ~domains ~alloc ~clients ~rounds =
  let lat = Array.make_matrix clients rounds 0L in
  Weft_linux.run ~domains (fun () ->
      let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
      let a = Net.Tcp ("127.0.0.1", Weft_linux.local_port l) in
      let t0 = Unix.gettimeofday () in
      let w0 = R.Timestamp.(to_int64 (get_current ())) in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              Fiber.scope (fun conns ->
                  while true do
                    let c, _ = Net.accept l in
                    Fiber.fork conns (fun () ->
                        let buf = Bytes.create 64 in
                        let rec loop () =
                          match Net.read c buf 0 64 with
                          | 0 ->
                              Net.close c
                          | k ->
                              if alloc then (
                                let junk = List.init 64 (fun i -> (i, k)) in
                                ignore (Sys.opaque_identity junk) ;
                                let n = Array.length !live in
                                !live.(Random.int n) <-
                                  Some (Bytes.sub_string buf 0 k) ) ;
                              ignore (Net.write_sub c buf 0 k) ;
                              loop ()
                        in
                        loop () )
                  done ) ) ;
          Fiber.scope (fun cs ->
              for i = 0 to clients - 1 do
                Fiber.fork cs (fun () ->
                    let c = Net.connect a in
                    let msg = String.make 64 'x' and buf = Bytes.create 64 in
                    for j = 0 to rounds - 1 do
                      let s0 = Time.now Mono in
                      Net.write c msg ;
                      let rec got n =
                        if n < 64 then got (n + Net.read c buf n (64 - n))
                      in
                      got 0 ;
                      lat.(i).(j) <-
                        Span.to_ns (Time.diff Mono (Time.now Mono) s0)
                    done ;
                    Net.close c )
              done ) ) ;
      let w1 = R.Timestamp.(to_int64 (get_current ())) in
      Net.close l ;
      let secs = Unix.gettimeofday () -. t0 in
      ( float (clients * rounds) /. secs
      , Array.concat (Array.to_list lat)
      , (w0, w1) ) )

let header () =
  Printf.printf "  %7s %10s %9s %9s %9s %9s %7s %9s %9s %9s %6s %4s\n" "domains"
    "trips/s" "trip p50" "trip p99" "trip p999" "trip max" "minors" "minor p50"
    "minor p99" "minor max" "GC %" "dog"

let measure cursor ~alloc ~domains =
  let s =
    { open_= Hashtbl.create 64
    ; spans= Hashtbl.create 8
    ; lost= 0
    ; window= (Int64.max_int, Int64.min_int) }
  in
  Gc.full_major () ;
  (* The events of the earlier runs. *)
  ignore (R.read_poll cursor (callbacks s) None) ;
  let dog0 = Weft_linux.watchdog_reports () in
  let trips, lat, window = echo ~domains ~alloc ~clients:64 ~rounds:2000 in
  s.window <- window ;
  let wall = Int64.to_float (Int64.sub (snd window) (fst window)) /. 1e9 in
  ignore (R.read_poll cursor (callbacks s) None) ;
  let get p = Option.value ~default:[] (Hashtbl.find_opt s.spans p) in
  let minor = sorted (get R.EV_MINOR) and slices = get R.EV_MAJOR_SLICE in
  let total l = List.fold_left Int64.add 0L l in
  let gc_ns = Int64.add (total (Array.to_list minor)) (total slices) in
  let share = 100. *. Int64.to_float gc_ns /. (wall *. 1e9 *. float domains) in
  Array.sort compare lat ;
  Printf.printf
    "  %7d %10.0f %9.0f %9.0f %9.0f %9.0f %7d %9.0f %9.0f %9.0f %6.1f %4d\n"
    domains trips
    (us (percentile 0.5 lat))
    (us (percentile 0.99 lat))
    (us (percentile 0.999 lat))
    (us lat.(Array.length lat - 1))
    (Array.length minor)
    (us (percentile 0.5 minor))
    (us (percentile 0.99 minor))
    (us (percentile 1. minor))
    share
    (Weft_linux.watchdog_reports () - dog0) ;
  let slices = sorted slices and leader = sorted (get R.EV_STW_LEADER) in
  Printf.printf
    "  %7s major slices: %d, p99 %.0f us, max %.0f us; STW leader: p99 %.0f \
     us, max %.0f us%s\n\
     %!"
    "" (Array.length slices)
    (us (percentile 0.99 slices))
    (us (percentile 1. slices))
    (us (percentile 0.99 leader))
    (us (percentile 1. leader))
    (if s.lost > 0 then Printf.sprintf "; %d EVENTS LOST" s.lost else "")

let () =
  Printf.printf "CPUs: %d\n" (Domain.recommended_domain_count ()) ;
  R.start () ;
  let cursor = R.create_cursor None in
  print_endline
    "Q6: echo, 64 clients, 2000 round trips each (64 bytes), on 127.0.0.1. \
     Times in us; minor: the span of each minor GC on each domain; GC %: the \
     minor GCs and major slices, of the time of all domains; dog: the watchdog \
     reports" ;
  print_endline "Load 1: no allocation in the server" ;
  header () ;
  List.iter (fun domains -> measure cursor ~alloc:false ~domains) [1; 2; 4; 8] ;
  live := Array.init 1_000_000 (fun i -> Some (string_of_int i)) ;
  print_endline
    "Load 2: a live heap of 1 million strings, and about 2 KB of allocation \
     for each request" ;
  header () ;
  List.iter (fun domains -> measure cursor ~alloc:true ~domains) [1; 2; 4; 8] ;
  R.free_cursor cursor
