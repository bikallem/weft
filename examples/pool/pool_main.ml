(* The pool on the OS: dune exec examples/pool/pool_main.exe. Job [i] sleeps for
   [i] times 100 ms, and each call waits 450 ms at most. A rate limit lets 5
   calls start each second. *)

open Weft

let job i =
  Time.sleep Mono (Span.ms (i * 100)) ;
  i * i

let () =
  Weft.run
    (module Weft_os)
    (fun () ->
      Fiber.scope (fun s ->
          let pool = Pool.create s ~workers:3 job in
          let rate = Pool.Rate.create s ~per_s:5 ~burst:1 in
          let t0 = Time.now Mono in
          Fiber.scope (fun calls ->
              for i = 1 to 8 do
                Pool.Rate.take rate ;
                Fiber.fork calls (fun () ->
                    let ms =
                      Int64.div
                        (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
                        1_000_000L
                    in
                    match Pool.call_timeout pool (Span.ms 450) i with
                    | Some v ->
                        Printf.printf "job %d (start at %4Ld ms): %d\n%!" i ms v
                    | None ->
                        Printf.printf
                          "job %d (start at %4Ld ms): no reply in 450 ms, the \
                           worker stopped it\n\
                           %!"
                          i ms )
              done ) ) )
