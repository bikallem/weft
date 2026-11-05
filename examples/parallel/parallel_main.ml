(* Count the primes on all domains: dune exec
   examples/parallel/parallel_main.exe [n]. The count must end in 5 s, else the
   program stops it. *)

open Weft

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 5_000_000
  in
  Weft.run
    (module Weft_os)
    (fun () ->
      let t0 = Time.now Mono in
      match
        Fiber.with_timeout Mono (Span.s 5) (fun () ->
            Parallel.primes_below ~tasks:16 n )
      with
      | count ->
          let ms =
            Int64.div
              (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
              1_000_000L
          in
          Printf.printf "%d primes below %d, in %Ld ms\n" count n ms
      | exception Fiber.Timeout ->
          print_endline "the count did not end in 5 s" )
