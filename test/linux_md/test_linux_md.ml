(* The tests of many domains on Linux (design section 2.14, M1). *)

open Windtrap
open Weft
module S = Private.Sched

let domains = 4

let world =
  { Portable.name= "linux on 4 domains"
  ; run= (fun f -> Weft_linux.run ~domains f)
  ; listen=
      (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        (l, Net.Tcp ("127.0.0.1", Weft_linux.local_port l)) ) }

let dom () = S.prog "test" S.Dom_index

(* Fibers that start on domain 0 run on other domains too, and each one ends. *)
let work_stealing () =
  Weft_linux.run ~domains (fun () ->
      let seen = Array.make domains (Atomic.make false) in
      Array.iteri (fun i _ -> seen.(i) <- Atomic.make false) seen ;
      let ends = Atomic.make 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 64 do
            Fiber.fork s (fun () ->
                for _ = 1 to 200 do
                  Atomic.set seen.(dom ()) true ;
                  Fiber.yield ()
                done ;
                Atomic.incr ends )
          done ) ;
      equal int 64 (Atomic.get ends) ;
      greater int ~than:1
        (Array.fold_left (fun n a -> if Atomic.get a then n + 1 else n) 0 seen) )

(* Timers on many domains end after their deadlines. *)
let timers () =
  Weft_linux.run ~domains (fun () ->
      Fiber.scope (fun s ->
          for i = 1 to 16 do
            Fiber.fork s (fun () ->
                let t0 = Time.now Mono in
                Time.sleep Mono (Span.ms (i mod 5)) ;
                at_least int64
                  ~than:(Int64.of_int (i mod 5 * 1_000_000))
                  (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )
          done ) )

(* A fiber that blocks on one fd from two domains in sequence gets all of its
   data. *)
let fd_from_two_domains () =
  let two = ref false in
  for _ = 1 to 20 do
    Weft_linux.run ~domains (fun () ->
        let l, a = world.listen () in
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
                  let ds = Atomic.get c.S.doms in
                  if ds land (ds - 1) <> 0 then two := true ;
                  Fiber.yield () ;
                  loop ()
            in
            loop () ;
            equal string "12345678901234567890" (Buffer.contents b) ;
            Net.close c ) ;
        Net.close l )
  done ;
  (* With 4 domains, a thief takes the reader at least one time. *)
  is_true !two

let open_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* 2,000 connections on 4 domains, and no fd stays open. *)
let no_fd_leak () =
  let before = open_fds () in
  Weft_linux.run ~domains (fun () ->
      let l, a = world.listen () in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Portable.echo_server l) ;
          Fiber.scope (fun clients ->
              for _ = 1 to 8 do
                Fiber.fork clients (fun () ->
                    for _ = 1 to 250 do
                      let c = Net.connect a in
                      Net.write c "x" ;
                      equal string "x" (Portable.read_n c 1) ;
                      Net.close c
                    done )
              done ) ) ;
      Net.close l ) ;
  equal int before (open_fds ())

let () =
  exit
    (Windtrap.run "linux_md"
       ( Portable.tests world
       @ [ group "many domains"
             [ test "work stealing: fibers run on more than one domain"
                 work_stealing
             ; test "timers on many domains end after their deadlines" timers
             ; test
                 "a fiber that blocks on one fd from two domains gets all data"
                 fd_from_two_domains
             ; test "2,000 connections on 4 domains with no fd leak" no_fd_leak
             ] ] ) )
