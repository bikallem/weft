(* Issue 36: a fiber that yields on a domain with busy I/O fibers. Run: dune
   exec bench/yield_io.exe -- K SECONDS

   K pairs of Unix sockets send 16 bytes back and forth with no pause, on one
   domain. One more fiber yields in a loop. The probe gives the yields of that
   fiber, the gaps between two of its yields, and the mean lengths of the
   overflow queue and of the ring at each yield (design section 2.5, The cost of
   a yield). *)

open Weft
module S = Private.Sched

let ping c buf =
  let rec fill off =
    if off < 16 then
      let k = Net.read c buf off (16 - off) in
      if k = 0 then raise Exit else fill (off + k)
  in
  fill 0

let probe k secs =
  let sched = ref None in
  let w = Weft_linux.create ~domains:1 () in
  let stop = Atomic.make false
  and rounds = Atomic.make 0
  and yields = ref 0
  and gaps = ref []
  and over = ref 0
  and over_max = ref 0
  and ring = ref 0 in
  let path = Filename.temp_file "yield_io" ".sock" in
  Sys.remove path ;
  Fun.protect
    ~finally:(fun () -> Weft_linux.close w)
    (fun () ->
      Private.run_world ~domains:1
        ~on_create:(fun s -> sched := Some s)
        (module Weft_linux.World)
        w
        (fun () ->
          let t = Option.get !sched in
          let l = Net.listen ~backlog:4096 (Net.Unix path) in
          Fiber.scope (fun s ->
              for _ = 1 to k do
                let c = Net.connect (Net.Unix path) in
                let sv, _ = Net.accept l in
                let msg = String.make 16 'x' in
                Fiber.fork s (fun () ->
                    let buf = Bytes.create 16 in
                    try
                      while not (Atomic.get stop) do
                        Net.write c msg ; ping c buf ; Atomic.incr rounds
                      done ;
                      Net.close c
                    with Exit | Net.Error _ -> Net.close c ) ;
                Fiber.fork s (fun () ->
                    let buf = Bytes.create 16 in
                    try
                      while true do
                        ping sv buf ; Net.write sv msg
                      done
                    with Exit | Net.Error _ -> Net.close sv )
              done ;
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.ms 200) ;
                  let last = ref (Unix.gettimeofday ()) in
                  while not (Atomic.get stop) do
                    Fiber.yield () ;
                    let now = Unix.gettimeofday () in
                    gaps := (now -. !last) :: !gaps ;
                    last := now ;
                    incr yields ;
                    let d = t.S.doms.(0) in
                    ring :=
                      !ring + Atomic.get d.S.ring.tail
                      - Atomic.get d.S.ring.head ;
                    let n = Queue.length t.S.overflow in
                    over := !over + n ;
                    over_max := Int.max !over_max n
                  done ) ;
              Time.sleep Mono (Span.ms (200 + int_of_float (secs *. 1000.))) ;
              Atomic.set stop true ) ;
          Net.close l ) ) ;
  let gaps = Array.of_list !gaps in
  Array.sort compare gaps ;
  let n = Array.length gaps in
  let pct p = if n = 0 then nan else gaps.(min (n - 1) (n * p / 100)) *. 1e3 in
  Printf.printf
    "k=%d yields=%d rounds=%d gap ms: p50=%.3f p99=%.3f max=%.3f overflow: \
     mean=%.0f max=%d ring: mean=%.0f mean gap ms=%.3f\n\
     %!"
    k n (Atomic.get rounds) (pct 50) (pct 99)
    (if n = 0 then nan else gaps.(n - 1) *. 1e3)
    (if n = 0 then 0. else float !over /. float n)
    !over_max
    (if n = 0 then 0. else float !ring /. float n)
    (if n = 0 then nan else Array.fold_left ( +. ) 0. gaps /. float n *. 1e3)

let () =
  let k = int_of_string Sys.argv.(1) and secs = float_of_string Sys.argv.(2) in
  probe k secs
