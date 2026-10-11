open Windtrap
open Weft

type world =
  { name: string
  ; domains: int
  ; run: 'a. (unit -> 'a) -> 'a
  ; listen: unit -> Net.t * Net.addr }

exception Boom of string

(* A log that fibers on many domains can add to. *)
module Log = struct
  let create () = Atomic.make []

  let rec add l x =
    let old = Atomic.get l in
    if not (Atomic.compare_and_set l old (x :: old)) then add l x

  (* The entries, the oldest first. *)
  let get l = List.rev (Atomic.get l)
end

let read_n c n =
  let buf = Bytes.create n in
  let rec loop off =
    if off = n then off
    else match Net.read c buf off (n - off) with 0 -> off | k -> loop (off + k)
  in
  Bytes.sub_string buf 0 (loop 0)

let read_all c =
  let b = Buffer.create 64 and buf = Bytes.create 4096 in
  let rec loop () =
    match Net.read c buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | k ->
        Buffer.add_subbytes b buf 0 k ;
        loop ()
  in
  loop ()

let echo_server l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork ~name:"echo" s (fun () ->
            (* Close the socket also on a cancel. *)
            Fun.protect
              ~finally:(fun () -> Net.close c)
              (fun () ->
                let buf = Bytes.create 4096 in
                let rec loop () =
                  match Net.read c buf 0 4096 with
                  | 0 ->
                      ()
                  | k ->
                      ignore (Net.write_sub c buf 0 k) ;
                      loop ()
                in
                loop () ) )
      done )

let error_of f =
  match f () with
  | _ ->
      "no error"
  | exception Net.Error (e, _) ->
      Format.asprintf "%a" Private.Op.pp_error e

let events w =
  group "events"
    [ test "always, never, wrap and guard" (fun () ->
          w.run (fun () ->
              equal int 4 (sync (choose [never; always 4])) ;
              equal int 5 (sync (wrap (always 2) (fun x -> x + 3))) ;
              let n = ref 0 in
              let ev = guard (fun () -> incr n ; always !n) in
              equal int 1 (sync ev) ;
              equal int 2 (sync ev) ) )
    ; test "the nack of a branch that lost is ready" (fun () ->
          w.run (fun () ->
              let nack = ref None in
              sync
                (choose
                   [ with_nack (fun n ->
                         nack := Some n ;
                         never )
                   ; always () ] ) ;
              match !nack with Some n -> sync n | None -> failwith "no guard" ) )
    ; test "an Ivar gives its value to a waiter" (fun () ->
          w.run (fun () ->
              let iv = Ivar.create () in
              Fiber.scope (fun s ->
                  let p =
                    Fiber.fork_promise s (fun () -> sync (Ivar.read iv))
                  in
                  Fiber.fork s (fun () -> Ivar.fill iv 42) ;
                  equal int 42 (Promise.await p) ) ) )
    ; test "two fibers that yield keep their own order" (fun () ->
          (* On many domains, the two fibers can run at the same time: only the
             order of each fiber is sure. *)
          let l = Log.create () in
          w.run (fun () ->
              Fiber.scope (fun s ->
                  List.iter
                    (fun name ->
                      Fiber.fork s (fun () ->
                          for i = 1 to 2 do
                            Log.add l (Printf.sprintf "%s%d" name i) ;
                            Fiber.yield ()
                          done ) )
                    ["a"; "b"] ) ) ;
          let all = Log.get l in
          let only p = List.filter (fun x -> x.[0] = p) all in
          equal (list string) ["a1"; "a2"] (only 'a') ;
          equal (list string) ["b1"; "b2"] (only 'b') )
    ; test "a sleep ends after its deadline" (fun () ->
          w.run (fun () ->
              let t0 = Time.now Mono in
              Time.sleep Mono (Span.ms 5) ;
              at_least int64 ~than:5_000_000L
                (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) ) )
    ; test "choose commits to the first deadline" (fun () ->
          w.run (fun () ->
              equal string "near"
                (sync
                   (choose
                      [ wrap (Time.after Mono (Span.ms 200)) (fun () -> "far")
                      ; wrap (Time.after Mono (Span.ms 5)) (fun () -> "near") ] ) ) ) )
    ]

let chans w =
  group "channels"
    [ test "a send and a recv meet" (fun () ->
          w.run (fun () ->
              let c = Chan.create () in
              Fiber.scope (fun s ->
                  let r = Fiber.fork_promise s (fun () -> sync (Chan.recv c)) in
                  Fiber.fork s (fun () -> sync (Chan.send c "hello")) ;
                  equal string "hello" (Promise.await r) ) ) )
    ; test "each sent value arrives one time" (fun () ->
          let got =
            w.run (fun () ->
                let c = Chan.create () and got = Log.create () in
                Fiber.scope (fun s ->
                    for i = 1 to 20 do
                      Fiber.fork s (fun () -> sync (Chan.send c i)) ;
                      Fiber.fork s (fun () -> Log.add got (sync (Chan.recv c)))
                    done ) ;
                Log.get got )
          in
          equal (list int) (List.init 20 succ) (List.sort compare got) )
    ; test "a sync never meets its own offer" (fun () ->
          w.run (fun () ->
              let c = Chan.create () in
              equal string "timeout"
                (sync
                   (choose
                      [ wrap (Chan.send c 1) (fun () -> "met")
                      ; wrap (Chan.recv c) (fun _ -> "met")
                      ; wrap (Time.after Mono (Span.ms 5)) (fun () -> "timeout")
                      ] ) ) ) ) ]

(* More than 256 fibers that yield: the fiber in the overflow queue runs (the
   review of M1, T1). *)
let overflow_runs w =
  test "a fiber in the overflow queue runs while the ring is full" (fun () ->
      w.run (fun () ->
          let flag = Atomic.make false in
          Fiber.scope (fun s ->
              (* The first fiber: a full ring moves its oldest half, with this
                 fiber, to the overflow queue. *)
              Fiber.fork s (fun () -> Atomic.set flag true) ;
              for _ = 1 to 300 do
                Fiber.fork s (fun () ->
                    let n = ref 0 in
                    while not (Atomic.get flag) do
                      incr n ;
                      if !n > 1_000_000 then failwith "starved" ;
                      Fiber.yield ()
                    done )
              done ) ) )

(* The tests of many domains (milestone M1). They run only when the world has
   more than one domain. *)

let dom () = Private.Sched.prog "test" Private.Sched.Dom_index

(* Fibers that start on domain 0 run on other domains too, and each one ends. *)
let work_stealing w () =
  let any = ref false in
  for _ = 1 to 20 do
    w.run (fun () ->
        let seen = Array.init w.domains (fun _ -> Atomic.make false) in
        let ends = Atomic.make 0 in
        Fiber.scope (fun s ->
            for _ = 1 to 40 do
              Fiber.fork s (fun () ->
                  for _ = 1 to 50 do
                    Atomic.set seen.(dom ()) true ;
                    Fiber.yield ()
                  done ;
                  Atomic.incr ends )
            done ) ;
        equal int 40 (Atomic.get ends) ;
        if
          Array.exists
            (fun a -> Atomic.get a)
            (Array.sub seen 1 (w.domains - 1))
        then any := true )
  done ;
  is_true !any

(* A fiber that blocks on one fd from two domains in sequence gets all of its
   data. After the close, a sleep gives the simulator a quiescence, so that a
   close notice that waits is a lost wakeup. *)
let fd_from_two_domains w () =
  let two = ref false in
  for _ = 1 to 50 do
    w.run (fun () ->
        let l, a = w.listen () in
        Fiber.scope (fun s ->
            Fiber.fork s (fun () ->
                let c, _ = Net.accept l in
                for i = 1 to 20 do
                  Net.write c (string_of_int (i mod 10)) ;
                  Time.sleep Mono (Span.ms 1)
                done ;
                Net.close c ) ;
            let c : Private.Net.t = Net.connect a in
            let b = Buffer.create 20 and buf = Bytes.create 8 in
            let rec loop () =
              match Net.read c buf 0 8 with
              | 0 ->
                  ()
              | k ->
                  Buffer.add_subbytes b buf 0 k ;
                  let ds = Atomic.get c.Private.Sched.doms in
                  if ds land (ds - 1) <> 0 then two := true ;
                  (* A yield lets a thief take the fiber. *)
                  Fiber.yield () ;
                  loop ()
            in
            loop () ;
            equal string "12345678901234567890" (Buffer.contents b) ;
            Net.close c ;
            Time.sleep Mono (Span.ms 1) ) ;
        Net.close l )
  done ;
  is_true !two

let many w =
  if w.domains = 1 then []
  else
    [ group "many domains"
        [ test "work stealing: fibers run on more than one domain"
            (work_stealing w)
        ; test "a fiber that blocks on one fd from two domains gets all data"
            (fd_from_two_domains w) ] ]

let scopes w =
  group "scopes"
    [ test "an error in a child cancels the body" (fun () ->
          w.run (fun () ->
              raises (Boom "child") (fun () ->
                  Fiber.scope (fun s ->
                      Fiber.fork s (fun () -> raise (Boom "child")) ;
                      Time.sleep Mono (Span.s 10) ) ) ) )
    ; test "a cancel goes down to a nested scope" (fun () ->
          w.run (fun () ->
              let seen = ref "" in
              Fiber.scope (fun outer ->
                  Fiber.fork outer (fun () ->
                      try Fiber.scope (fun _ -> Time.sleep Mono (Span.s 10))
                      with Fiber.Cancelled (Boom m) -> seen := m ) ;
                  Fiber.yield () ;
                  Fiber.cancel outer (Boom "stop") ) ;
              equal string "stop" !seen ) )
    ; test "the cleanup in a shield runs after a cancel" (fun () ->
          let sent = ref false in
          w.run (fun () ->
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      Fun.protect
                        (fun () -> Time.sleep Mono (Span.s 10))
                        ~finally:(fun () ->
                          Fiber.shield (fun () ->
                              Fiber.with_timeout Mono (Span.s 1) (fun () ->
                                  Time.sleep Mono (Span.ms 2) ;
                                  sent := true ) ) ) ) ;
                  Fiber.yield () ;
                  Fiber.cancel s (Boom "close") ) ) ;
          is_true !sent )
    ; test "two failures give Multiple" (fun () ->
          w.run (fun () ->
              match
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () -> raise (Boom "a")) ;
                    Fiber.fork s (fun () -> raise (Boom "b")) )
              with
              | () ->
                  failwith "no exception"
              | exception Fiber.Multiple l ->
                  equal int 2 (List.length l) ) )
    ; test "with_timeout raises Timeout" (fun () ->
          w.run (fun () ->
              raises Fiber.Timeout (fun () ->
                  Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                      Time.sleep Mono (Span.s 10) ) ) ) )
    ; test "a fork into an ended scope raises Scope_closed" (fun () ->
          w.run (fun () ->
              let saved = ref None in
              Fiber.scope (fun s -> saved := Some s) ;
              match !saved with
              | Some s ->
                  raises Fiber.Scope_closed (fun () -> Fiber.fork s ignore)
              | None ->
                  failwith "no scope" ) )
    ; test "a promise holds the exception" (fun () ->
          w.run (fun () ->
              Fiber.scope (fun s ->
                  let p = Fiber.fork_promise s (fun () -> raise (Boom "p")) in
                  raises (Boom "p") (fun () -> Promise.await p) ) ) ) ]

let net w =
  group "net"
    [ test "echo: the client gets hello back" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () -> echo_server l) ;
                  let c = Net.connect a in
                  Net.write c "hello" ;
                  equal string "hello" (read_n c 5) ;
                  Net.close c ) ) )
    ; test "after the peer closes: the end of the stream, then EPIPE" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      let c, _ = Net.accept l in
                      Net.write c "bye" ; Net.close c ) ;
                  let c = Net.connect a in
                  equal string "bye" (read_all c) ;
                  (* On Linux, the first write after the close of the peer can
                     succeed, and a later one gives EPIPE or ECONNRESET. *)
                  let rec until_error n =
                    if n = 0 then "no error"
                    else
                      match Net.write c "x" with
                      | () ->
                          Time.sleep Mono (Span.ms 1) ;
                          until_error (n - 1)
                      | exception Net.Error (e, _) ->
                          Format.asprintf "%a" Private.Op.pp_error e
                  in
                  mem string (until_error 100) ["EPIPE"; "ECONNRESET"] ;
                  Net.close c ) ) )
    ; test "close gives EBADF to a fiber that waits" (fun () ->
          w.run (fun () ->
              let l, _ = w.listen () in
              Fiber.scope (fun s ->
                  let p =
                    Fiber.fork_promise s (fun () ->
                        error_of (fun () -> Net.accept l) )
                  in
                  Fiber.yield () ;
                  Net.close l ;
                  equal string "EBADF" (Promise.await p) ) ) )
    ; test "a late write after close gives EBADF and closes nothing" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () -> echo_server l) ;
                  let c = Net.connect a in
                  Net.close c ;
                  (* The next socket can get the same fd number. *)
                  let d = Net.connect a in
                  equal string "EBADF" (error_of (fun () -> Net.write c "late")) ;
                  Net.write d "ok" ;
                  equal string "ok" (read_n d 2) ;
                  Net.close d ) ) )
    ; test "shutdown gives the end of the stream to the peer" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  let p =
                    Fiber.fork_promise s (fun () ->
                        let c, _ = Net.accept l in
                        let r = read_all c in
                        Net.write c (String.uppercase_ascii r) ;
                        Net.close c ;
                        r )
                  in
                  let c = Net.connect a in
                  Net.write c "ping" ;
                  Net.shutdown c ;
                  equal string "PING" (read_all c) ;
                  equal string "ping" (Promise.await p) ;
                  Net.close c ) ) )
    ; test "a connect after the listener closed gives ECONNREFUSED" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Net.close l ;
              equal string "ECONNREFUSED" (error_of (fun () -> Net.connect a)) ) )
    ; test "a listen on an address in use gives EADDRINUSE" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              equal string "EADDRINUSE" (error_of (fun () -> Net.listen a)) ;
              Net.close l ) )
    ; test "1 MiB goes through and comes back" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              let data =
                String.init (1 lsl 20) (fun i -> Char.chr (i land 0xff))
              in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () -> echo_server l) ;
                  let c = Net.connect a in
                  let r =
                    Fiber.fork_promise s (fun () ->
                        read_n c (String.length data) )
                  in
                  Net.write c data ;
                  is_true (String.equal data (Promise.await r)) ;
                  Net.close c ) ) ) ]

let tests w =
  [group w.name ([events w; chans w; scopes w; net w; overflow_runs w] @ many w)]
