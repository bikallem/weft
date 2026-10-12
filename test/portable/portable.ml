open Windtrap
open Weft

type world =
  { name: string
  ; domains: int
  ; run: 'a. (unit -> 'a) -> 'a
  ; listen: unit -> Net.t * Net.addr
  ; udp: unit -> Net.t * (string * int)
  ; tmp: unit -> string }

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

(* Read until the end of the stream. *)
let drain read r =
  let b = Buffer.create 64 and buf = Bytes.create 4096 in
  let rec loop () =
    match read r buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | k ->
        Buffer.add_subbytes b buf 0 k ;
        loop ()
  in
  loop ()

let read_all = drain Net.read

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
                      (* A write can move only a part of the bytes. *)
                      let rec send off =
                        if off < k then
                          send (off + Net.write_sub c buf off (k - off))
                      in
                      send 0 ; loop ()
                in
                loop () ) )
      done )

let error_of f =
  match f () with
  | _ ->
      "no error"
  | exception Net.Error (e, _) ->
      Format.asprintf "%a" Private.Op.pp_error e

let host_port s =
  match Net.local_addr s with
  | Net.Tcp (host, port) ->
      (host, port)
  | Net.Unix _ ->
      invalid_arg "host_port"

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
          w.run (fun () ->
              let l = Log.create () in
              Fiber.scope (fun s ->
                  List.iter
                    (fun name ->
                      Fiber.fork s (fun () ->
                          for i = 1 to 2 do
                            Log.add l (Printf.sprintf "%s%d" name i) ;
                            Fiber.yield ()
                          done ) )
                    ["a"; "b"] ) ;
              let only p = List.filter (fun x -> x.[0] = p) (Log.get l) in
              equal (list string) ["a1"; "a2"] (only 'a') ;
              equal (list string) ["b1"; "b2"] (only 'b') ) )
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

(* [read_now], [write_now] and [reset]. *)

(* Read with [readable] and [read_now] until the end of the stream. The limit of
   the stale events stops a loop that spins: in the simulator, the time does not
   move while a fiber spins. *)
let read_now_all c =
  let b = Buffer.create 64 and buf = Bytes.create 4096 and stale = ref 0 in
  let rec loop () =
    sync (Net.readable c) ;
    match Net.read_now c buf 0 4096 with
    | None ->
        incr stale ;
        if !stale > 1000 then failwith "the loop spins" ;
        loop ()
    | Some 0 ->
        Buffer.contents b
    | Some k ->
        Buffer.add_subbytes b buf 0 k ;
        loop ()
  in
  loop ()

let read_error c = error_of (fun () -> Net.read c (Bytes.create 16) 0 16)

(* Write until a write gives an error, at most 100 times: the reset of the peer
   can come later. Then give the text of the error. *)
let first_write_error c =
  let rec go n =
    if n = 0 then "no error"
    else
      match Net.write c "x" with
      | () ->
          Time.sleep Mono (Span.ms 1) ;
          go (n - 1)
      | exception Net.Error (e, _) ->
          Format.asprintf "%a" Private.Op.pp_error e
  in
  go 100

(* A connection, and a server fiber that runs [f] on the accepted socket. The
   server starts only after [connect] gave the socket: if the reset came during
   the connect, [connect] would raise ECONNRESET. The client starts only after
   [accept] gave the socket: on FreeBSD, an accept of a connection that the
   client reset in the backlog gives ECONNABORTED. *)
let with_conn w f client =
  let l, a = w.listen () in
  let connected = Ivar.create () and accepted = Ivar.create () in
  Fiber.scope (fun s ->
      Fiber.fork s (fun () ->
          let c, _ = Net.accept l in
          Ivar.fill accepted () ;
          sync (Ivar.read connected) ;
          f c ) ;
      let c = Net.connect a in
      Ivar.fill connected () ;
      sync (Ivar.read accepted) ;
      client c ;
      Net.close c ) ;
  Net.close l

let now_and_reset w =
  group "read_now, write_now and reset"
    [ test "read_now gives None, then the data, then the end of the stream"
        (fun () ->
          w.run (fun () ->
              let go = Ivar.create () in
              with_conn w
                (fun c ->
                  sync (Ivar.read go) ;
                  Net.write c "hello" ;
                  Net.close c )
                (fun c ->
                  equal (option int) None (Net.read_now c (Bytes.create 4) 0 4) ;
                  Ivar.fill go () ;
                  equal string "hello" (read_now_all c) ) ) )
    ; test "a loop of readable and read_now waits for a new edge" (fun () ->
          w.run (fun () ->
              let go = Ivar.create () in
              with_conn w
                (fun c ->
                  sync (Ivar.read go) ;
                  Net.write c "x" ;
                  ignore (read_all c) ;
                  Net.close c )
                (fun c ->
                  (* The read of another fiber takes the data with no EAGAIN, so
                     the read flag of its domain stays set. The worlds with one
                     domain find a loop that spins. On more domains, the other
                     fiber can run on another domain, and the test then proves
                     less. *)
                  Fiber.scope (fun s ->
                      let b = Fiber.fork_promise s (fun () -> read_n c 1) in
                      Fiber.yield () ;
                      Ivar.fill go () ;
                      equal string "x" (Promise.await b) ) ;
                  let stop =
                    Option.get (Time.add Mono (Time.now Mono) (Span.ms 20))
                  in
                  let buf = Bytes.create 1 in
                  (* Without a clear of the flag after None, the loop spins
                     until the limit. *)
                  let rec loop n =
                    if n >= 1000 then n
                    else
                      match
                        sync
                          (choose
                             [ wrap (Net.readable c) (fun () -> `Input)
                             ; wrap (Time.at Mono stop) (fun () -> `Stop) ] )
                      with
                      | `Stop ->
                          n
                      | `Input -> (
                        match Net.read_now c buf 0 1 with
                        | None ->
                            loop (n + 1)
                        | Some k ->
                            failwith (Printf.sprintf "read_now gave %d" k) )
                  in
                  less int ~than:50 (loop 0) ) ) )
    ; test "write_now gives None when the socket is full" (fun () ->
          w.run (fun () ->
              let go = Ivar.create () and got = Ivar.create () in
              with_conn w
                (fun c ->
                  sync (Ivar.read go) ;
                  Ivar.fill got (String.length (read_all c)) ;
                  Net.close c )
                (fun c ->
                  let chunk = Bytes.make 65536 'x' in
                  let rec fill total =
                    match Net.write_now c chunk 0 65536 with
                    | Some k ->
                        fill (total + k)
                    | None ->
                        total
                  in
                  let total = fill 0 in
                  at_least int ~than:1 total ;
                  Ivar.fill go () ;
                  (* After None, [writable] waits until the reader takes
                     data. *)
                  sync (Net.writable c) ;
                  Net.shutdown c ;
                  equal int total (sync (Ivar.read got)) ) ) )
    ; test "reset gives ECONNRESET to the peer, then the end of the stream"
        (fun () ->
          w.run (fun () ->
              with_conn w Net.reset (fun c ->
                  equal string "ECONNRESET" (read_error c) ;
                  equal string "" (read_all c) ) ) )
    ; test "a write after a reset of the peer gives an error" (fun () ->
          w.run (fun () ->
              with_conn w Net.reset (fun c ->
                  (* Linux gives ECONNRESET, and FreeBSD can give EPIPE. *)
                  mem string (first_write_error c) ["ECONNRESET"; "EPIPE"] ) ) )
    ; test "reset gives EBADF to a fiber that waits, and RST to the peer"
        (fun () ->
          w.run (fun () ->
              let peer = Ivar.create () in
              with_conn w
                (fun c ->
                  Ivar.fill peer (read_error c) ;
                  Net.close c )
                (fun c ->
                  Fiber.scope (fun s ->
                      let p = Fiber.fork_promise s (fun () -> read_error c) in
                      Fiber.yield () ;
                      Net.reset c ;
                      equal string "EBADF" (Promise.await p) ) ;
                  equal string "ECONNRESET" (sync (Ivar.read peer)) ) ) )
    ; test "a reset after the peer closed, and a second reset, raise nothing"
        (fun () ->
          w.run (fun () ->
              with_conn w Net.close (fun c ->
                  equal string "" (read_all c) ;
                  Net.reset c ;
                  Net.reset c ;
                  equal string "EBADF" (read_error c) ) ) )
    ; test "a client that resets before the accept does not stop the accepts"
        (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              let go = Ivar.create () in
              (* Linux and the simulator give the connection that reset, and its
                 read gives ECONNRESET. macOS and FreeBSD give ECONNABORTED, and
                 accept takes the next connection. *)
              let first c =
                match read_n c 1 with s -> s | exception Net.Error _ -> ""
              in
              let rec accept_b () =
                let c, _ = Net.accept l in
                let s = first c in
                Net.close c ;
                if s <> "b" then accept_b ()
              in
              Fiber.scope (fun s ->
                  let got =
                    Fiber.fork_promise s (fun () ->
                        sync (Ivar.read go) ;
                        accept_b () )
                  in
                  Net.reset (Net.connect a) ;
                  let c = Net.connect a in
                  Net.write c "b" ;
                  Time.sleep Mono (Span.ms 10) ;
                  Ivar.fill go () ;
                  Promise.await got ;
                  Net.close c ) ;
              Net.close l ) )
    ; test "a reset of a listener closes it" (fun () ->
          w.run (fun () ->
              let l, _ = w.listen () in
              Net.reset l ;
              equal string "EBADF" (error_of (fun () -> Net.accept l)) ) ) ]

(* A Unix connection. The client writes data that the server does not read, then
   the server runs [close] on its socket. Then the client runs [client]. *)
let unix_unread w close client =
  let a = Net.Unix (Filename.concat (w.tmp ()) "u") in
  let l = Net.listen a in
  let sent = Ivar.create () and closed = Ivar.create () in
  Fiber.scope (fun s ->
      Fiber.fork s (fun () ->
          let c, _ = Net.accept l in
          sync (Ivar.read sent) ;
          close c ;
          Ivar.fill closed () ) ;
      let c = Net.connect a in
      Net.write c "unread" ;
      Ivar.fill sent () ;
      sync (Ivar.read closed) ;
      Time.sleep Mono (Span.ms 10) ;
      client c ;
      Net.close c ) ;
  Net.close l

(* The details of a reset that follow Linux. The simulator and the Linux world
   run them. *)
let reset_details w =
  let pause () = Time.sleep Mono (Span.ms 10) in
  group
    (w.name ^ ": reset, as on Linux")
    [ test "the peer reads the data before the reset" (fun () ->
          w.run (fun () ->
              with_conn w
                (fun c -> Net.write c "data" ; pause () ; Net.reset c)
                (fun c ->
                  equal string "data" (read_n c 4) ;
                  equal string "ECONNRESET" (read_error c) ;
                  equal string "" (read_all c) ) ) )
    ; test "a write after the reset gives ECONNRESET, then EPIPE" (fun () ->
          w.run (fun () ->
              with_conn w Net.reset (fun c ->
                  equal string "ECONNRESET" (first_write_error c) ;
                  equal string "EPIPE" (error_of (fun () -> Net.write c "x")) ;
                  equal string "" (read_all c) ) ) )
    ; test "a reset after shutdown gives the end of the stream and EPIPE"
        (fun () ->
          w.run (fun () ->
              let reset = Ivar.create () in
              with_conn w
                (fun c ->
                  Net.shutdown c ; pause () ; Net.reset c ; Ivar.fill reset () )
                (fun c ->
                  equal string "" (read_all c) ;
                  sync (Ivar.read reset) ;
                  pause () ;
                  equal string "EPIPE" (error_of (fun () -> Net.write c "x")) ) ) )
    ; test "a close with unread data sends a reset" (fun () ->
          w.run (fun () ->
              let sent = Ivar.create () in
              with_conn w
                (fun c ->
                  sync (Ivar.read sent) ;
                  pause () ;
                  Net.close c )
                (fun c ->
                  Net.write c "unread" ;
                  Ivar.fill sent () ;
                  equal string "ECONNRESET" (read_error c) ;
                  equal string "" (read_all c) ) ) )
    ; test "a reset of a Unix socket gives the end of the stream" (fun () ->
          w.run (fun () ->
              let a = Net.Unix (Filename.concat (w.tmp ()) "s") in
              let l = Net.listen a in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      let c, _ = Net.accept l in
                      Net.reset c ) ;
                  let c = Net.connect a in
                  equal string "" (read_all c) ;
                  Net.close c ) ;
              Net.close l ) )
    ; test "a Unix socket with unread data: a close resets the reads only"
        (fun () ->
          w.run (fun () ->
              unix_unread w
                (fun c -> Net.close c)
                (fun c ->
                  (* A write gives EPIPE and leaves the error for the read. *)
                  let write () = error_of (fun () -> Net.write c "x") in
                  equal string "EPIPE" (write ()) ;
                  equal string "EPIPE" (write ()) ;
                  equal string "ECONNRESET" (read_error c) ;
                  equal string "" (read_all c) ) ) )
    ; test "a close of a listener resets the TCP connections of its backlog"
        (fun () ->
          w.run (fun () ->
              let read_first c =
                equal string "ECONNRESET" (read_error c) ;
                equal string "" (read_all c)
              and write_first c =
                let write () = error_of (fun () -> Net.write c "x") in
                equal string "ECONNRESET" (write ()) ;
                equal string "EPIPE" (write ()) ;
                equal string "" (read_all c)
              in
              List.iter
                (fun client ->
                  let l, a = w.listen () in
                  let c = Net.connect a in
                  Net.close l ; pause () ; client c ; Net.close c )
                [read_first; write_first] ) )
    ; test "a close of a listener resets the Unix connections of its backlog"
        (fun () ->
          w.run (fun () ->
              let dir = w.tmp () in
              List.iteri
                (fun i client ->
                  let a = Net.Unix (Filename.concat dir (string_of_int i)) in
                  let l = Net.listen a in
                  let c = Net.connect a in
                  Net.close l ; pause () ; client c ; Net.close c )
                [ (fun c ->
                    equal string "ECONNRESET" (read_error c) ;
                    equal string "" (read_all c) )
                ; (fun c ->
                    (* A write gives EPIPE and leaves the error for the read. *)
                    let write () = error_of (fun () -> Net.write c "x") in
                    equal string "EPIPE" (write ()) ;
                    equal string "EPIPE" (write ()) ;
                    equal string "ECONNRESET" (read_error c) ;
                    equal string "" (read_all c) ) ] ) )
    ; test "a Unix socket with unread data: a reset after shutdown" (fun () ->
          w.run (fun () ->
              unix_unread w
                (fun c -> Net.shutdown c ; Net.reset c)
                (fun c ->
                  equal string "ECONNRESET" (read_error c) ;
                  equal string "" (read_all c) ) ) ) ]

(* UDP and the socket options. A world can lose datagrams (the faults world), so
   a test sends again after a timeout. *)

(* Send [msg] to [dst] until [f] accepts a datagram that comes back, at most 100
   times. *)
let until_answer s dst msg f =
  let buf = Bytes.create 64 in
  let rec go n =
    if n = 0 then fail "no answer after 100 datagrams" ;
    Net.sendto s (Bytes.of_string msg) 0 (String.length msg) dst ;
    match
      Fiber.with_timeout Mono (Span.ms 50) (fun () ->
          let k, from = Net.recvfrom s buf 0 (Bytes.length buf) in
          f (Bytes.sub_string buf 0 k) from )
    with
    | true ->
        ()
    | false | (exception Fiber.Timeout) ->
        go (n - 1)
  in
  go 100

(* The host of the addresses of the world. *)
let host_of w =
  let l, a = w.listen () in
  Net.close l ;
  match a with Net.Tcp (h, _) -> h | Net.Unix _ -> invalid_arg "host_of"

let local_addr w =
  group "local_addr"
    [ test "a listen on port 0 gets a port, and a client connects to it"
        (fun () ->
          w.run (fun () ->
              let host = host_of w in
              let l = Net.listen (Net.Tcp (host, 0)) in
              let a = Net.local_addr l in
              ( match a with
              | Net.Tcp (h, p) ->
                  equal string host h ;
                  is_true (p > 0)
              | Net.Unix _ ->
                  fail "a Unix address" ) ;
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () -> echo_server l) ;
                  let c = Net.connect a in
                  Net.write c "hello" ;
                  equal string "hello" (read_n c 5) ;
                  Net.close c ) ;
              Net.close l ) )
    ; test "an accepted socket and its client agree on the two addresses"
        (fun () ->
          w.run (fun () ->
              let l = Net.listen (Net.Tcp (host_of w, 0)) in
              let a = Net.local_addr l in
              let c = Net.connect a in
              let s, peer = Net.accept l in
              is_true (Net.local_addr s = a) ;
              is_true (Net.local_addr c = peer) ;
              ( match peer with
              | Net.Tcp (_, p) ->
                  is_true (p > 0)
              | Net.Unix _ ->
                  fail "a Unix address" ) ;
              (* The client keeps its port after the close of the peer. *)
              Net.close s ;
              is_true (Net.local_addr c = peer) ;
              Net.close c ;
              Net.close l ) )
    ; test "a Unix listener has its path, and a Unix client has no path"
        (fun () ->
          w.run (fun () ->
              let a = Net.Unix (Filename.concat (w.tmp ()) "s") in
              let l = Net.listen a in
              let c = Net.connect a in
              let s, peer = Net.accept l in
              is_true (Net.local_addr l = a) ;
              is_true (Net.local_addr s = a) ;
              is_true (Net.local_addr c = Net.Unix "") ;
              is_true (peer = Net.Unix "") ;
              Net.close s ;
              Net.close c ;
              Net.close l ) )
    ; test "a UDP socket on port 0 gets a port" (fun () ->
          w.run (fun () ->
              let u, u_addr = w.udp () in
              equal (pair string int) u_addr (host_port u) ;
              let host = fst u_addr in
              let a = Net.udp (host, 0) and b = Net.udp (host, 0) in
              let ha, pa = host_port a and hb, pb = host_port b in
              equal string host ha ;
              equal string host hb ;
              is_true (pa > 0 && pb > 0 && pa <> pb) ;
              Net.close a ;
              Net.close b ;
              Net.close u ) )
    ; test "a closed socket gives EBADF" (fun () ->
          w.run (fun () ->
              let l, _ = w.listen () and u, _ = w.udp () in
              Net.close l ;
              Net.close u ;
              equal string "EBADF" (error_of (fun () -> Net.local_addr l)) ;
              equal string "EBADF" (error_of (fun () -> Net.local_addr u)) ) )
    ]

let udp w =
  group "udp"
    [ test "a datagram gets its answer, with the addresses of the two sides"
        (fun () ->
          w.run (fun () ->
              let srv, srv_addr = w.udp () and cli, cli_addr = w.udp () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () ->
                      let buf = Bytes.create 64 in
                      while true do
                        let k, from = Net.recvfrom srv buf 0 64 in
                        is_true (from = cli_addr) ;
                        let r = Bytes.uppercase_ascii (Bytes.sub buf 0 k) in
                        Net.sendto srv r 0 k from
                      done ) ;
                  until_answer cli srv_addr "ping" (fun data from ->
                      is_true (from = srv_addr) ;
                      String.equal data "PING" ) ) ;
              Net.close srv ;
              Net.close cli ) )
    ; test "a datagram longer than the buffer loses its other bytes" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () and b, _ = w.udp () in
              let buf = Bytes.create 4 in
              let recv () =
                Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                    let k, _ = Net.recvfrom a buf 0 4 in
                    Bytes.sub_string buf 0 k )
              in
              let rec first n =
                if n = 0 then fail "no datagram" ;
                Net.sendto b (Bytes.of_string "0123456789") 0 10 a_addr ;
                match recv () with
                | r ->
                    equal string "0123" r
                | exception Fiber.Timeout ->
                    first (n - 1)
              in
              first 100 ;
              (* A late copy of the first datagram can come, but never its other
                 bytes. *)
              let rec next n =
                if n = 0 then fail "no second datagram" ;
                Net.sendto b (Bytes.of_string "ab") 0 2 a_addr ;
                match recv () with
                | "ab" ->
                    ()
                | r ->
                    equal string "0123" r ;
                    next (n - 1)
                | exception Fiber.Timeout ->
                    next (n - 1)
              in
              next 100 ; Net.close a ; Net.close b ) )
    ; test "a datagram to a closed socket is lost, with no error" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () and b, _ = w.udp () in
              Net.close a ;
              Net.sendto b (Bytes.of_string "x") 0 1 a_addr ;
              Net.close b ) )
    ; test "a UDP socket on an address in use gives EADDRINUSE" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () in
              equal string "EADDRINUSE" (error_of (fun () -> Net.udp a_addr)) ;
              Net.close a ) )
    ; test "a closed UDP socket gives EBADF" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () in
              Net.close a ;
              equal string "EBADF"
                (error_of (fun () ->
                     Net.sendto a (Bytes.of_string "x") 0 1 a_addr ) ) ;
              equal string "EBADF"
                (error_of (fun () -> Net.recvfrom a (Bytes.create 1) 0 1)) ) )
    ; test "Net.write on a UDP socket with no peer gives EDESTADDRREQ"
        (fun () ->
          w.run (fun () ->
              let a, _ = w.udp () in
              equal string "EDESTADDRREQ" (error_of (fun () -> Net.write a "x")) ;
              Net.close a ) )
    ; test "a datagram of more than 65507 bytes gives EMSGSIZE" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () in
              equal string "EMSGSIZE"
                (error_of (fun () ->
                     Net.sendto a (Bytes.create 65508) 0 65508 a_addr ) ) ;
              Net.close a ) )
    ; test "the smallest receive buffer loses datagrams" (fun () ->
          w.run (fun () ->
              let a, a_addr = w.udp () and b, _ = w.udp () in
              Net.set_option a Recv_buffer 1 ;
              for _ = 1 to 10 do
                Net.sendto b (Bytes.make 1000 'x') 0 1000 a_addr
              done ;
              Time.sleep Mono (Span.ms 20) ;
              let buf = Bytes.create 1000 in
              let rec count n =
                match
                  Fiber.with_timeout Mono (Span.ms 20) (fun () ->
                      Net.recvfrom a buf 0 1000 )
                with
                | _ ->
                    count (n + 1)
                | exception Fiber.Timeout ->
                    n
              in
              (* Linux 7.1 keeps 1 of 10, Linux 6.x keeps 2, and macOS keeps
                 none: the cost of a datagram depends on the kernel. *)
              less int ~than:5 (count 0) ;
              Net.close a ;
              Net.close b ) )
    ; test "sendto and recvfrom need a UDP socket" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  let p = Fiber.fork_promise s (fun () -> fst (Net.accept l)) in
                  let c = Net.connect a in
                  let invalid = function
                    | Invalid_argument _ ->
                        true
                    | _ ->
                        false
                  in
                  raises_match invalid (fun () ->
                      Net.sendto c (Bytes.create 1) 0 1 ("x", 1) ) ;
                  raises_match invalid (fun () ->
                      Net.recvfrom c (Bytes.create 1) 0 1 ) ;
                  Net.close c ;
                  Net.close (Promise.await p) ) ;
              Net.close l ) )
    ; test "the options of a TCP socket and of a UDP socket" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Fiber.scope (fun s ->
                  let p = Fiber.fork_promise s (fun () -> fst (Net.accept l)) in
                  let c = Net.connect a in
                  let srv = Promise.await p in
                  is_false (Net.get_option c No_delay) ;
                  Net.set_option c No_delay true ;
                  is_true (Net.get_option c No_delay) ;
                  Net.set_option c Keep_alive true ;
                  is_true (Net.get_option c Keep_alive) ;
                  Net.set_option c Recv_buffer 100_000 ;
                  at_least int ~than:100_000 (Net.get_option c Recv_buffer) ;
                  Net.set_option c Send_buffer 100_000 ;
                  at_least int ~than:100_000 (Net.get_option c Send_buffer) ;
                  raises_match
                    (function Invalid_argument _ -> true | _ -> false)
                    (fun () -> Net.set_option c Recv_buffer 0) ;
                  Net.close c ;
                  Net.close srv ) ;
              Net.close l ;
              let u, _ = w.udp () in
              Net.set_option u Broadcast true ;
              is_true (Net.get_option u Broadcast) ;
              (* ENOPROTOOPT on Linux, EINVAL on macOS. *)
              is_false
                (String.equal "no error"
                   (error_of (fun () -> Net.set_option u No_delay true)) ) ;
              Net.close u ;
              equal string "EBADF"
                (error_of (fun () -> Net.get_option u Broadcast)) ) )
    ; test "an accepted socket gets the options of its listener" (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              Net.set_option l Keep_alive true ;
              Fiber.scope (fun s ->
                  let p = Fiber.fork_promise s (fun () -> fst (Net.accept l)) in
                  let c = Net.connect a in
                  let srv = Promise.await p in
                  is_true (Net.get_option srv Keep_alive) ;
                  is_false (Net.get_option c Keep_alive) ;
                  Net.close c ;
                  Net.close srv ) ;
              Net.close l ) ) ]

(* The fiber layer of milestone M2: the handlers of a scope, fork_isolated,
   Promise.cancel, and the locks for fiber code. *)

type _ Effect.t += Count : int -> unit Effect.t

(* A handler that adds each [Count n] to [total]. *)
let counting total =
  { Fiber.handle=
      (fun f ->
        Effect.Deep.match_with f ()
          { retc= Fun.id
          ; exnc= raise
          ; effc=
              (fun (type a) (e : a Effect.t) ->
                match e with
                | Count n ->
                    Some
                      (fun (k : (a, _) Effect.Deep.continuation) ->
                        ignore (Atomic.fetch_and_add total n) ;
                        Effect.Deep.continue k () )
                | _ ->
                    None ) } ) }

(* A handler that doubles each [Count n], and gives it to the outer handler. *)
let doubling =
  { Fiber.handle=
      (fun f ->
        Effect.Deep.match_with f ()
          { retc= Fun.id
          ; exnc= raise
          ; effc=
              (fun (type a) (e : a Effect.t) ->
                match e with
                | Count n ->
                    Some
                      (fun (k : (a, _) Effect.Deep.continuation) ->
                        Effect.perform (Count (2 * n)) ;
                        Effect.Deep.continue k () )
                | _ ->
                    None ) } ) }

(* A server that forks each connection with fork_isolated. A connection that
   sends "boom" raises. *)
let isolated_server l errors =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork_isolated s
          ~on_error:(fun e _ -> Log.add errors (Printexc.to_string e))
          (fun () ->
            Fun.protect
              ~finally:(fun () -> Net.close c)
              (fun () ->
                let m = read_n c 4 in
                if m = "boom" then raise (Boom m) ;
                Time.sleep Mono (Span.ms 2) ;
                Net.write c m ) )
      done )

let layers w =
  group "fiber layers"
    [ test "a user effect from a nested scope reaches the outer handler"
        (fun () ->
          w.run (fun () ->
              let total = Atomic.make 0 in
              Fiber.scope ~handler:(counting total) (fun outer ->
                  Effect.perform (Count 1) ;
                  Fiber.fork outer (fun () ->
                      Fiber.yield () ; Effect.perform (Count 10) ) ;
                  Fiber.scope ~handler:doubling (fun inner ->
                      Effect.perform (Count 100) ;
                      Fiber.fork inner (fun () ->
                          Fiber.yield () ;
                          Effect.perform (Count 1000) ) ;
                      (* A fiber of the outer scope does not run inside the
                         handler of the inner scope. *)
                      Fiber.fork outer (fun () ->
                          Fiber.yield () ;
                          Effect.perform (Count 10_000) ) ) ) ;
              equal int (1 + 10 + 200 + 2000 + 10_000) (Atomic.get total) ) )
    ; test "an isolated connection that raises does not stop the others"
        (fun () ->
          w.run (fun () ->
              let l, a = w.listen () in
              let errors = Log.create () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () -> isolated_server l errors) ;
                  Fiber.scope (fun clients ->
                      List.iter
                        (fun m ->
                          Fiber.fork clients (fun () ->
                              let c = Net.connect a in
                              Net.write c m ;
                              equal string
                                (if m = "boom" then "" else m)
                                (read_all c) ;
                              Net.close c ) )
                        ["aaaa"; "boom"; "cccc"; "dddd"] ) ) ;
              Net.close l ;
              equal (list string)
                [Printexc.to_string (Boom "boom")]
                (Log.get errors) ) )
    ; test "on_error that raises makes the scope fail" (fun () ->
          w.run (fun () ->
              raises (Boom "again") (fun () ->
                  Fiber.scope (fun s ->
                      Fiber.fork_isolated s
                        ~on_error:(fun _ _ -> raise (Boom "again"))
                        (fun () -> raise (Boom "first")) ) ) ) )
    ; test "Promise.cancel cancels only the fiber of the promise" (fun () ->
          w.run (fun () ->
              Fiber.scope (fun s ->
                  let early =
                    Fiber.fork_promise s (fun () ->
                        Time.sleep Mono (Span.s 10) ;
                        1 )
                  in
                  let late =
                    Fiber.fork_promise s (fun () ->
                        Time.sleep Mono (Span.s 10) ;
                        2 )
                  in
                  let other =
                    Fiber.fork_promise s (fun () ->
                        Time.sleep Mono (Span.ms 5) ;
                        3 )
                  in
                  (* Before the fiber starts, and while it sleeps. *)
                  Promise.cancel early ;
                  Time.sleep Mono (Span.ms 1) ;
                  Promise.cancel late ;
                  raises Promise.Cancelled (fun () -> Promise.await early) ;
                  raises Promise.Cancelled (fun () -> Promise.await late) ;
                  equal int 3 (Promise.await other) ;
                  Promise.cancel other ;
                  equal int 3 (Promise.await other) ) ) )
    ; test "a cancel of the outer scope gives Promise.Cancelled" (fun () ->
          w.run (fun () ->
              let p = ref None in
              ( try
                  Fiber.scope (fun s ->
                      p :=
                        Some
                          (Fiber.fork_promise s (fun () ->
                               Time.sleep Mono (Span.s 10) ) ) ;
                      Fiber.cancel s (Boom "stop") ;
                      Time.sleep Mono (Span.s 10) )
                with Fiber.Cancelled (Boom _) -> () ) ;
              match !p with
              | Some p ->
                  raises Promise.Cancelled (fun () -> Promise.await p)
              | None ->
                  failwith "no promise" ) )
      (* A fiber on another domain makes the promise, and its fiber fails on
         domain 0. Then the run ends, and the other domains terminate. The
         finaliser of the failure must not hold a value of their minor heaps. *)
    ; test "a promise that fails on another domain than its fork" (fun () ->
          for _ = 1 to 200 do
            w.run (fun () ->
                Fiber.scope (fun s ->
                    for _ = 1 to 8 do
                      Fiber.fork s (fun () ->
                          Fiber.yield () ;
                          if (Domain.self () :> int) <> 0 then
                            let p =
                              Fiber.fork_promise ~pinned:true s (fun () ->
                                  raise (Boom "p") )
                            in
                            raises (Boom "p") (fun () -> Promise.await p) )
                    done ) )
          done )
    ; test "two fibers that wake each other do not starve the queue" (fun () ->
          w.run (fun () ->
              let a = Chan.create () and b = Chan.create () in
              let flag = Atomic.make false in
              Fiber.scope (fun s ->
                  (* The first fiber decides when the two stop, and the second
                     one always answers. *)
                  Fiber.fork s (fun () ->
                      let n = ref 0 in
                      while not (Atomic.get flag) do
                        incr n ;
                        if !n > 100_000 then failwith "starved" ;
                        sync (Chan.send a true) ;
                        sync (Chan.recv b)
                      done ;
                      sync (Chan.send a false) ) ;
                  Fiber.fork s (fun () ->
                      while sync (Chan.recv a) do
                        sync (Chan.send b ())
                      done ) ;
                  (* The fiber in the queue behind the two. *)
                  Fiber.fork s (fun () -> Atomic.set flag true) ) ) )
    ; test "a mutex keeps the counter correct" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and n = ref 0 in
              Fiber.scope (fun s ->
                  for _ = 1 to 20 do
                    Fiber.fork s (fun () ->
                        for _ = 1 to 10 do
                          Fiber.Mutex.use_rw ~protect:false m (fun () ->
                              let x = !n in
                              Fiber.yield () ;
                              n := x + 1 )
                        done )
                  done ) ;
              equal int 200 !n ) )
    ; test "an exception poisons the mutex, also for the waiters" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () in
              Fiber.scope (fun s ->
                  let waiter =
                    Fiber.fork_promise s (fun () ->
                        Time.sleep Mono (Span.ms 1) ;
                        Fiber.Mutex.use_ro m ignore )
                  in
                  raises (Boom "rw") (fun () ->
                      Fiber.Mutex.use_rw ~protect:false m (fun () ->
                          Time.sleep Mono (Span.ms 5) ;
                          raise (Boom "rw") ) ) ;
                  raises Fiber.Mutex.Poisoned (fun () -> Promise.await waiter) ;
                  raises Fiber.Mutex.Poisoned (fun () -> Fiber.Mutex.lock m) ) ) )
    ; test "use_ro never poisons the mutex" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () in
              raises (Boom "ro") (fun () ->
                  Fiber.Mutex.use_ro m (fun () -> raise (Boom "ro")) ) ;
              equal int 1 (Fiber.Mutex.use_rw ~protect:false m (fun () -> 1)) ) )
    ; test "use_rw ~protect:true ends f after a cancel" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and ended = ref false in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      Fiber.Mutex.use_rw ~protect:true m (fun () ->
                          Time.sleep Mono (Span.ms 5) ;
                          ended := true ) ;
                      Time.sleep Mono (Span.s 10) ) ;
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.cancel s (Boom "stop") ) ;
              is_true !ended ;
              Fiber.Mutex.use_ro m ignore ) )
    ; test "a fiber that leaves the wait for a lock does not take it" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () in
              Fiber.scope (fun s ->
                  Fiber.Mutex.lock m ;
                  let p = Fiber.fork_promise s (fun () -> Fiber.Mutex.lock m) in
                  Time.sleep Mono (Span.ms 1) ;
                  Promise.cancel p ;
                  raises Promise.Cancelled (fun () -> Promise.await p) ;
                  Fiber.Mutex.unlock m ;
                  Fiber.Mutex.lock m ;
                  Fiber.Mutex.unlock m ) ) )
    ; test "a condition gives each item to one consumer" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              let q = Queue.create () and got = Log.create () in
              Fiber.scope (fun s ->
                  for _ = 1 to 4 do
                    Fiber.fork s (fun () ->
                        for _ = 1 to 5 do
                          let x =
                            Fiber.Mutex.use_rw ~protect:false m (fun () ->
                                while Queue.is_empty q do
                                  Fiber.Condition.wait c m
                                done ;
                                Queue.pop q )
                          in
                          Log.add got x
                        done )
                  done ;
                  for i = 1 to 20 do
                    Fiber.Mutex.use_rw ~protect:false m (fun () ->
                        Queue.push i q ; Fiber.Condition.signal c ) ;
                    if i mod 3 = 0 then Fiber.yield ()
                  done ) ;
              equal (list int) (List.init 20 succ)
                (List.sort compare (Log.get got)) ) )
    ; test "signal wakes one waiter" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              let woken = Atomic.make 0 in
              Fiber.scope (fun s ->
                  for _ = 1 to 2 do
                    Fiber.fork s (fun () ->
                        Fiber.Mutex.use_rw ~protect:false m (fun () ->
                            Fiber.Condition.wait c m ) ;
                        Atomic.incr woken )
                  done ;
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.Mutex.use_rw ~protect:false m (fun () ->
                      Fiber.Condition.signal c ) ;
                  while Atomic.get woken = 0 do
                    Fiber.yield ()
                  done ;
                  Time.sleep Mono (Span.ms 2) ;
                  equal int 1 (Atomic.get woken) ;
                  Fiber.Mutex.use_rw ~protect:false m (fun () ->
                      Fiber.Condition.broadcast c ) ) ) )
    ; test "broadcast wakes each waiter" (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              let go = ref false and woken = Atomic.make 0 in
              Fiber.scope (fun s ->
                  for _ = 1 to 5 do
                    Fiber.fork s (fun () ->
                        Fiber.Mutex.use_rw ~protect:false m (fun () ->
                            while not !go do
                              Fiber.Condition.wait c m
                            done ) ;
                        Atomic.incr woken )
                  done ;
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.Mutex.use_rw ~protect:false m (fun () ->
                      go := true ;
                      Fiber.Condition.broadcast c ) ) ;
              equal int 5 (Atomic.get woken) ) )
    ; test "a cancel in Condition.wait does not poison, also with protect"
        (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              Fiber.scope (fun s ->
                  List.iter
                    (fun protect ->
                      Fiber.fork s (fun () ->
                          Fiber.Mutex.use_rw ~protect m (fun () ->
                              Fiber.Condition.wait c m ) ) )
                    [false; true] ;
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.cancel s (Boom "stop") ) ;
              equal int 2 (Fiber.Mutex.use_rw ~protect:false m (fun () -> 2)) ) )
    ; test "R1: a timeout inside use_rw ~protect:true ends Condition.wait"
        (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              raises Fiber.Timeout (fun () ->
                  Fiber.Mutex.use_rw ~protect:true m (fun () ->
                      Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                          Fiber.Condition.wait c m ) ) ) ) )
    ; test "R2: a wait in use_ro does not use the shield of another holder"
        (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
              Fiber.scope (fun s ->
                  Fiber.fork_daemon s (fun () ->
                      Fiber.Mutex.use_rw ~protect:true m (fun () ->
                          Fiber.Condition.wait c m ) ) ;
                  Time.sleep Mono (Span.ms 1) ;
                  raises Fiber.Timeout (fun () ->
                      Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                          Fiber.Mutex.use_ro m (fun () ->
                              Fiber.Condition.wait c m ) ) ) ) ) )
    ; test "R3: Mutex.lock is a cancellation point when the mutex is free"
        (fun () ->
          w.run (fun () ->
              let m = Fiber.Mutex.create () in
              Fiber.scope (fun s ->
                  Fiber.cancel s (Boom "stop") ;
                  raises (Fiber.Cancelled (Boom "stop")) (fun () ->
                      Fiber.Mutex.lock m ) ) ;
              Fiber.Mutex.lock m ;
              Fiber.Mutex.unlock m ) ) ]

(* Files and directories (milestone M2). *)

(* [File.Error] is [Net.Error]. *)
let fs_error = error_of

let write_file path data =
  let f = File.openfile [Write; Create; Truncate] path in
  File.write f data ; File.close f

let read_file path =
  let f = File.openfile [Read] path in
  let b = Buffer.create 64 and buf = Bytes.create 7 in
  let rec loop () =
    match File.read f buf 0 7 with
    | 0 ->
        ()
    | k ->
        Buffer.add_subbytes b buf 0 k ;
        loop ()
  in
  loop () ; File.close f ; Buffer.contents b

let files w =
  group "files"
    [ test "a file gives back its data" (fun () ->
          w.run (fun () ->
              let path = Filename.concat (w.tmp ()) "a" in
              write_file path "hello world" ;
              equal string "hello world" (read_file path) ;
              let f = File.openfile [Read] path in
              let buf = Bytes.make 8 '.' in
              equal int 5 (File.pread f ~file_offset:6 buf 1 7) ;
              equal string ".world.." (Bytes.to_string buf) ;
              (* pread does not move the offset of the file. *)
              equal int 4 (File.read f buf 0 4) ;
              equal string "hell" (Bytes.sub_string buf 0 4) ;
              let st = File.fstat f in
              equal int 11 st.size ;
              is_true (st.kind = File.Regular) ;
              File.close f ;
              is_true ((File.stat path).kind = File.Regular) ) )
    ; test "pwrite, truncate and append" (fun () ->
          w.run (fun () ->
              let path = Filename.concat (w.tmp ()) "b" in
              write_file path "abcdef" ;
              let f = File.openfile [Read; Write] path in
              equal int 1
                (File.pwrite f ~file_offset:1 (Bytes.of_string "B") 0 1) ;
              File.truncate f 4 ;
              File.fsync f ;
              File.close f ;
              equal string "aBcd" (read_file path) ;
              let f = File.openfile [Write; Append] path in
              File.write f "XY" ;
              File.close f ;
              equal string "aBcdXY" (read_file path) ;
              equal int 6 (File.stat path).size ) )
    ; test "the directory operations" (fun () ->
          w.run (fun () ->
              let d = w.tmp () in
              let p = Filename.concat d in
              Dir.mkdir (p "sub") ;
              write_file (p "x") "1" ;
              write_file (p "y") "2" ;
              equal (list string) ["sub"; "x"; "y"] (Dir.readdir d) ;
              is_true ((File.stat (p "sub")).kind = File.Directory) ;
              Dir.rename (p "x") (p "sub/z") ;
              (* A rename on a file replaces it. *)
              Dir.rename (p "y") (p "sub/z") ;
              equal string "2" (read_file (p "sub/z")) ;
              equal (list string) ["sub"] (Dir.readdir d) ;
              Dir.unlink (p "sub/z") ;
              equal (list string) [] (Dir.readdir (p "sub")) ;
              Dir.rmdir (p "sub") ;
              equal (list string) [] (Dir.readdir d) ) )
    ; test "the errors of open question 1" (fun () ->
          w.run (fun () ->
              let d = w.tmp () in
              let p = Filename.concat d in
              write_file (p "f") "data" ;
              Dir.mkdir (p "dir") ;
              write_file (p "dir/g") "" ;
              let err = fs_error in
              equal string "ENOENT"
                (err (fun () -> File.openfile [Read] (p "no"))) ;
              equal string "ENOENT" (err (fun () -> File.stat (p "no/x"))) ;
              equal string "EEXIST"
                (err (fun () ->
                     File.openfile [Write; Create; Exclusive] (p "f") ) ) ;
              equal string "EISDIR"
                (err (fun () -> File.openfile [Write] (p "dir"))) ;
              equal string "ENOTDIR"
                (err (fun () -> File.openfile [Read] (p "f/x"))) ;
              equal string "EEXIST" (err (fun () -> Dir.mkdir (p "dir"))) ;
              equal string "ENOENT" (err (fun () -> Dir.mkdir (p "no/dir"))) ;
              equal string "ENOTEMPTY" (err (fun () -> Dir.rmdir (p "dir"))) ;
              equal string "ENOTDIR" (err (fun () -> Dir.rmdir (p "f"))) ;
              equal string "ENOENT" (err (fun () -> Dir.rmdir (p "no"))) ;
              equal string "EISDIR" (err (fun () -> Dir.unlink (p "dir"))) ;
              equal string "ENOENT" (err (fun () -> Dir.unlink (p "no"))) ;
              equal string "ENOTDIR" (err (fun () -> Dir.readdir (p "f"))) ;
              equal string "ENOENT" (err (fun () -> Dir.readdir (p "no"))) ;
              equal string "EISDIR"
                (err (fun () -> Dir.rename (p "f") (p "dir"))) ;
              equal string "ENOTDIR"
                (err (fun () -> Dir.rename (p "dir") (p "f"))) ;
              equal string "ENOENT" (err (fun () -> Dir.rename (p "no") (p "x"))) ;
              Dir.mkdir (p "empty") ;
              equal string "ENOTEMPTY"
                (err (fun () -> Dir.rename (p "empty") (p "dir"))) ;
              (* A write to a file that is open only for read, and a read after
                 the close. *)
              let f = File.openfile [Read] (p "f") in
              equal string "EBADF" (err (fun () -> File.write f "x")) ;
              File.close f ;
              equal string "EBADF"
                (err (fun () -> File.read f (Bytes.create 1) 0 1)) ) )
    ; test "C5 and C12: more errors agree with Linux" (fun () ->
          w.run (fun () ->
              let p = Filename.concat (w.tmp ()) in
              write_file (p "f") "data" ;
              Dir.mkdir (p "dir") ;
              (* The text of [Other] is not the same in each world. *)
              let err f =
                match fs_error f with
                | ( "ENOENT"
                  | "ENOTDIR"
                  | "EISDIR"
                  | "EEXIST"
                  | "ENOTEMPTY"
                  | "EBADF"
                  | "no error" ) as e ->
                    e
                | _ ->
                    "other"
              in
              let f = File.openfile [Read] (p "f") in
              equal int 0 (File.pread f ~file_offset:100 (Bytes.create 4) 0 4) ;
              equal string "other" (err (fun () -> File.truncate f 0)) ;
              File.close f ;
              equal string "ENOTDIR"
                (err (fun () -> File.openfile [Read] (p "f/"))) ;
              equal string "ENOTDIR" (err (fun () -> File.stat (p "f/"))) ;
              equal string "ENOTDIR" (err (fun () -> Dir.unlink (p "f/"))) ;
              equal string "EISDIR"
                (err (fun () -> File.openfile [Write; Create] (p "dir"))) ;
              equal string "ENOENT" (err (fun () -> File.openfile [Read] "")) ;
              equal string "other" (err (fun () -> Dir.rmdir (p "dir/."))) ;
              (* A directory cannot go into itself, and it can replace an empty
                 directory. *)
              Dir.mkdir (p "dir/sub") ;
              equal string "other"
                (err (fun () -> Dir.rename (p "dir") (p "dir/sub/x"))) ;
              Dir.mkdir (p "empty") ;
              Dir.rename (p "dir/sub") (p "empty") ;
              equal (list string) [] (Dir.readdir (p "dir")) ;
              is_true ((File.stat (p "empty")).kind = File.Directory) ) )
    ; test "pread and pwrite at offset 0 do not move the offset" (fun () ->
          w.run (fun () ->
              let path = Filename.concat (w.tmp ()) "o" in
              write_file path "abcdefgh" ;
              let f = File.openfile [Read; Write] path in
              let buf = Bytes.create 4 in
              equal int 4 (File.read f buf 0 4) ;
              equal int 2 (File.pread f ~file_offset:0 buf 0 2) ;
              equal string "ab" (Bytes.sub_string buf 0 2) ;
              equal int 1
                (File.pwrite f ~file_offset:0 (Bytes.of_string "A") 0 1) ;
              equal int 4 (File.read f buf 0 4) ;
              equal string "efgh" (Bytes.to_string buf) ;
              File.close f ;
              equal string "Abcdefgh" (read_file path) ;
              (* Two writes at the offset of the file follow each other. *)
              let f = File.openfile [Write; Truncate] path in
              File.write f "ab" ;
              File.write f "cd" ;
              File.close f ;
              equal string "abcd" (read_file path) ) )
    ; test "fibers use many files at the same time" (fun () ->
          w.run (fun () ->
              let d = w.tmp () in
              Fiber.scope (fun s ->
                  for i = 1 to 20 do
                    Fiber.fork s (fun () ->
                        let path = Filename.concat d (string_of_int i) in
                        write_file path (String.make i 'x') ;
                        equal int i (String.length (read_file path)) )
                  done ) ;
              equal int 20 (List.length (Dir.readdir d)) ) ) ]

(* Pipes (milestone M2). *)

let pipe_error = error_of

let pipe_read_all = drain Pipe.read

let pipes w =
  group "pipes"
    [ test "1 MiB goes through a pipe, then the end of the stream" (fun () ->
          w.run (fun () ->
              let r, wr = Pipe.create () in
              let data =
                String.init (1 lsl 20) (fun i -> Char.chr (i * 7 land 0xff))
              in
              Fiber.scope (fun s ->
                  let got = Fiber.fork_promise s (fun () -> pipe_read_all r) in
                  Pipe.write wr data ;
                  Pipe.close wr ;
                  is_true (String.equal data (Promise.await got)) ) ;
              Pipe.close r ) )
    ; test "a write after the reader closed gives EPIPE" (fun () ->
          w.run (fun () ->
              let r, wr = Pipe.create () in
              Pipe.close r ;
              equal string "EPIPE" (pipe_error (fun () -> Pipe.write wr "x")) ;
              Pipe.close wr ) )
    ; test "each end has one direction" (fun () ->
          w.run (fun () ->
              let r, wr = Pipe.create () in
              equal string "EBADF" (pipe_error (fun () -> Pipe.write r "x")) ;
              equal string "EBADF"
                (pipe_error (fun () -> Pipe.read wr (Bytes.create 1) 0 1)) ;
              Pipe.close r ;
              Pipe.close wr ) ) ]

let tests w =
  [ group w.name
      ( [ events w
        ; chans w
        ; scopes w
        ; net w
        ; now_and_reset w
        ; local_addr w
        ; udp w
        ; overflow_runs w
        ; layers w
        ; files w
        ; pipes w ]
      @ many w ) ]
