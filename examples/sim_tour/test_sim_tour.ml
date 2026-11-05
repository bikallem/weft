(* A tour of the simulator. The simulator runs the real code of the program with
   a virtual clock, virtual domains, and an in-memory network. A seed sets all
   of the choices: the order of the domains, the delays and the faults. Thus a
   seed always gives the same run, and a failure of a seed comes again with the
   same seed. *)

open Weft
module Sim = Weft_sim

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* 1. A deadlock. Each fiber waits for the other. When no fiber can run, no
   deadline exists and no event can come, [Sim.run] raises [Deadlock] with the
   names of the fibers that wait. *)
let deadlock () =
  match
    Sim.run ~domains:2 (fun _ ->
        let a = Ivar.create () and b = Ivar.create () in
        Fiber.scope (fun s ->
            Fiber.fork ~name:"left" s (fun () ->
                sync (Ivar.read a) ;
                Ivar.fill b () ) ;
            Fiber.fork ~name:"right" s (fun () ->
                sync (Ivar.read b) ;
                Ivar.fill a () ) ) )
  with
  | () ->
      assert false
  | exception Sim.Deadlock fibers ->
      assert (List.sort compare fibers = ["left"; "main"; "right"])

(* 2. Network faults. A server gives an answer to each line. A client sends 30
   requests, and it connects again after an error. The faults come from the
   seed: a random delay, short reads and writes, and a reset of a connection at
   one write in 64. *)

let read_line c =
  let b = Buffer.create 16 and byte = Bytes.create 1 in
  let rec loop () =
    match Net.read c byte 0 1 with
    | 0 ->
        None
    | _ when Bytes.get byte 0 = '\n' ->
        Some (Buffer.contents b)
    | _ ->
        Buffer.add_bytes b byte ; loop ()
  in
  loop ()

let server l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork_isolated s
          ~on_error:(fun _ _ -> ())
          (fun () ->
            Fun.protect
              ~finally:(fun () -> Net.close c)
              (fun () ->
                let rec loop () =
                  match read_line c with
                  | None ->
                      ()
                  | Some req ->
                      Net.write c ("ok " ^ req ^ "\n") ;
                      loop ()
                in
                loop () ) )
      done )

(* The requests, and the number of errors before each answer. The result is a
   trace of the run: the errors and the virtual time at the end. *)
let client a =
  let conn = ref None in
  let rec request i errors =
    let c =
      match !conn with
      | Some c ->
          c
      | None ->
          let c = Net.connect a in
          conn := Some c ;
          c
    in
    match
      Net.write c (Printf.sprintf "%d\n" i) ;
      read_line c
    with
    | Some answer ->
        assert (answer = Printf.sprintf "ok %d" i) ;
        errors
    | None | (exception Net.Error ((Reset | Broken_pipe), _)) ->
        Net.close c ;
        conn := None ;
        if errors >= 20 then failwith "too many errors" ;
        request i (errors + 1)
  in
  let t0 = Time.now Mono in
  let errors = List.init 30 (fun i -> request i 0) in
  Option.iter Net.close !conn ;
  (errors, ms_since t0)

let faults seed =
  Sim.run ~seed ~domains:2 (fun sim ->
      Sim.set_latency sim (Span.ms 2) ;
      Sim.net_faults sim
        {Sim.no_faults with jitter= Span.ms 3; short_io= true; resets= true} ;
      let a = Sim.addr sim "server:80" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> server l) ;
          client a ) )

let faults () =
  let total = ref 0 in
  for seed = 1 to 100 do
    let errors, _ = faults seed in
    total := !total + List.fold_left ( + ) 0 errors
  done ;
  (* The faults occurred, and each request got its answer. *)
  assert (!total > 0) ;
  (* The same seed gives the same run: the same errors at the same requests, and
     the same virtual time. To debug a failure, run its seed again. *)
  assert (faults 7 = faults 7)

(* 3. The manual clock. With [~auto:false], the time moves only in
   [Sim.advance_until]. Thus a test can examine the state at each time. A
   monitor marks a peer as dead when no heartbeat comes in 3 s. The peer sends a
   heartbeat each second, and it stops after 5 s. *)
let monitor beats alive =
  let rec loop () =
    match
      sync
        (choose
           [ wrap (Chan.recv beats) (fun () -> `Beat)
           ; wrap (Time.after Mono (Span.s 3)) (fun () -> `Silent) ] )
    with
    | `Beat ->
        loop ()
    | `Silent ->
        Var.set alive false
  in
  loop ()

let manual_clock () =
  Sim.run ~auto:false (fun sim ->
      let t0 = Time.now Mono in
      let at ms = Option.get (Time.add Mono t0 (Span.ms ms)) in
      let beats = Chan.create () and alive = Var.create true in
      Fiber.scope (fun s ->
          Fiber.fork ~name:"monitor" s (fun () -> monitor beats alive) ;
          Fiber.fork ~name:"peer" s (fun () ->
              for _ = 1 to 5 do
                Time.sleep Mono (Span.s 1) ;
                sync (Chan.send beats ())
              done ) ;
          Sim.advance_until sim (at 5000) ;
          assert (Var.get alive) ;
          (* The last heartbeat came at 5 s. A timer never ends before its
             deadline, but it can end up to 100 us after it (the slack of the
             world). Each sleep of the peer adds this delay. *)
          Sim.advance_until sim (at 7999) ;
          assert (Var.get alive) ;
          Sim.advance_until sim (at 8001) ;
          assert (not (Var.get alive)) ) )

(* 4. UDP with packet loss. One datagram in 8 is lost. The client sends each
   ping again after 100 ms with no answer. A late answer to an earlier try can
   come, thus the client ignores an answer with a different number. *)
let udp seed =
  Sim.run ~seed ~domains:2 (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      Sim.net_faults sim {Sim.no_faults with udp_loss= true} ;
      let server = Net.udp ("server", 53) and client = Net.udp ("client", 0) in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              let buf = Bytes.create 64 in
              while true do
                let n, from = Net.recvfrom server buf 0 64 in
                Net.sendto server buf 0 n from
              done ) ;
          let buf = Bytes.create 64 in
          let rec ping i tries =
            let msg = Bytes.of_string (string_of_int i) in
            Net.sendto client msg 0 (Bytes.length msg) ("server", 53) ;
            let rec answer () =
              let n, _ = Net.recvfrom client buf 0 64 in
              if Bytes.sub buf 0 n <> msg then answer ()
            in
            match Fiber.with_timeout Mono (Span.ms 100) answer with
            | () ->
                tries
            | exception Fiber.Timeout ->
                ping i (tries + 1)
          in
          List.fold_left ( + ) 0 (List.init 20 (fun i -> ping i 1)) - 20 )
      |> fun lost ->
      (* The scope cancelled the server, thus no fiber uses the sockets. *)
      Net.close client ; Net.close server ; lost )

let udp () =
  let lost = ref 0 in
  for seed = 1 to 50 do
    lost := !lost + udp seed
  done ;
  assert (!lost > 0)

let () = deadlock () ; faults () ; manual_clock () ; udp ()
