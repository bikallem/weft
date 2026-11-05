(* Structured concurrency: each fiber is in a scope, and the scope waits for all
   of its fibers. The code uses only the API of weft, so it runs in each
   world. *)

open Weft

(* Apply [f] to each element, each call in a fiber of its own. The fibers run on
   all of the domains of the world. If one call raises, the scope cancels the
   other calls, and [map] raises the error of that call.

   A cancel from an outer scope, for example of [Fiber.with_timeout], stops the
   calls. Their [Cancelled] is not a failure, thus the scope returns normally,
   but with no result for these calls. [Fiber.check_cancel] then raises the
   [Cancelled] of the outer scope. *)
let map f xs =
  let xs = Array.of_list xs in
  let ys = Array.make (Array.length xs) None in
  Fiber.scope (fun s ->
      Array.iteri (fun i x -> Fiber.fork s (fun () -> ys.(i) <- Some (f x))) xs ) ;
  Fiber.check_cancel () ;
  Array.to_list (Array.map Option.get ys)

(* Run each function in a promise, and give the value of the first promise that
   ends. Then cancel the other promises. The scope waits until they end, thus no
   fiber of [race] stays alive after it. If the first promise that ends raised,
   [race] raises the same exception. *)
let race fs =
  Fiber.scope (fun s ->
      let ps = List.map (fun f -> Fiber.fork_promise s f) fs in
      let v = sync (choose (List.map Promise.ready ps)) in
      List.iter Promise.cancel ps ;
      v )

(* Run [f], then [flush], also when a cancel stops [f]. A cancelled fiber raises
   [Cancelled] at each wait, thus [flush] runs in a shield: there, a wait does
   not see the cancel. Keep the shield short: the cancel waits for it. *)
let with_flush ~flush f = Fun.protect ~finally:(fun () -> Fiber.shield flush) f

(* The number of primes in the range [lo, hi). The loop does a yield after each
   1000 numbers. A yield lets the other fibers of the domain run, and it is a
   point where a cancel can stop the loop. Without it, the watchdog of the OS
   world writes a warning when the loop runs for more than 10 ms. *)
let count_primes lo hi =
  let is_prime n =
    let rec go d = d * d > n || (n mod d <> 0 && go (d + 1)) in
    n >= 2 && go 2
  in
  let count = ref 0 in
  for n = lo to hi - 1 do
    if is_prime n then incr count ;
    if n mod 1000 = 0 then Fiber.yield ()
  done ;
  !count

(* Cut the range [lo, hi) into [n] ranges of about the same size. *)
let ranges n lo hi =
  let size = (hi - lo + n - 1) / n in
  List.init n (fun i -> (lo + (i * size), min hi (lo + ((i + 1) * size))))
  |> List.filter (fun (a, b) -> a < b)

(* The number of primes below [n], in [tasks] fibers. *)
let primes_below ~tasks n =
  map (fun (lo, hi) -> count_primes lo hi) (ranges tasks 0 n)
  |> List.fold_left ( + ) 0
