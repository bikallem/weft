(* The reload in the simulator, on 2 virtual domains, for 30 seeds. The file is
   in the file system of the simulator, and the test sends SIGHUP with
   [Sim.signal]. *)

open Weft
module Sim = Weft_sim

let write path text =
  let f = File.openfile [Write; Create; Truncate] path in
  File.write f text ; File.close f

let () =
  for seed = 1 to 30 do
    Sim.run ~seed ~domains:2 ~signals:[SIGHUP] (fun sim ->
        let t0 = Time.now Mono in
        let ms_since () =
          Int64.to_int
            (Int64.div
               (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
               1_000_000L )
        in
        write "/app.conf" "period_ms=1000\nmessage=a\n" ;
        let config =
          match Reload.load "/app.conf" with
          | Ok c ->
              Var.create c
          | Error e ->
              failwith e
        in
        (* The worker sends each message on a channel. The test takes it, and
           makes sure of its time. *)
        let ticks = Chan.create () in
        let expect msg ms =
          let m = sync (Chan.recv ticks) in
          assert (m = msg && ms_since () = ms)
        in
        let errors = Atomic.make 0 in
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () ->
                Reload.watch "/app.conf" config ~on_error:(fun _ ->
                    Atomic.incr errors ) ) ;
            Fiber.fork_daemon s (fun () ->
                Reload.ticker config ~emit:(fun m -> sync (Chan.send ticks m)) ) ;
            expect "a" 1000 ;
            expect "a" 2000 ;
            (* At 2500 ms, a new period of 100 ms: the tick of 2100 ms is past,
               thus a tick comes at once. *)
            Time.sleep Mono (Span.ms 500) ;
            write "/app.conf" "period_ms=100\nmessage=b\n" ;
            Sim.signal sim SIGHUP ;
            expect "b" 2500 ;
            expect "b" 2600 ;
            (* A file that is not correct: the configuration stays. *)
            write "/app.conf" "period_ms=oops\nmessage=c\n" ;
            Sim.signal sim SIGHUP ;
            expect "b" 2700 ;
            expect "b" 2800 ;
            assert (Atomic.get errors = 1) ) )
  done
