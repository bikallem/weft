(* The read rule of Weft_flow.Flow on the TLS flow: a read that raises took no
   byte of the stream. Thus the plaintext and the ciphertext that the flow holds
   stay after a cancel, a Timeout and a Closed in a read. *)

open Windtrap
open Weft
open Weft_flow
open Support

(* [read_after_cancel ~seed ~domains ~delay] starts a read of the server in a
   fiber, sends "x" from the raw client, waits [delay] yields, and cancels the
   read. It gives the plaintext of the read, or of a later read if the cancel
   won. *)
let read_after_cancel ~seed ~domains ~delay =
  with_raw ~seed ~domains (fun server tcp tls ->
      Fiber.scope (fun s ->
          let reader = Fiber.fork_promise s (fun () -> read_string server 16) in
          Time.sleep Mono (Span.ms 1) ;
          ignore (send tcp tls "x") ;
          for _ = 1 to delay do
            Fiber.yield ()
          done ;
          Promise.cancel reader ;
          match Promise.await reader with
          | x ->
              x
          | exception Promise.Cancelled ->
              read_string server 16 ) )

(* [stream ~seed ~domains] sends 64 KiB in records of random sizes, at random
   times, then close_notify. A Reader of the server reads values of random
   sizes, each in a Fiber.with_timeout of a random time: many reads end with a
   cancel. It gives the bytes that the server read, and the bytes that the
   client sent. *)
let stream ~seed ~domains =
  let rnd = Random.State.make [|seed|] in
  let size = 65536 in
  let data =
    String.init size (fun i -> Char.chr (((i * 7) + (i / 251)) land 255))
  in
  with_raw ~seed ~domains (fun server tcp tls ->
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let rec go tls off =
                if off < size then (
                  let n = min (size - off) (1 + Random.State.int rnd 3000) in
                  let tls = send tcp tls (String.sub data off n) in
                  Time.sleep Mono (Span.us (Random.State.int rnd 3000)) ;
                  go tls (off + n) )
                else
                  let _, close = Tls.Engine.send_close_notify tls in
                  Flow.write_string tcp close
              in
              go tls 0 ) ;
          let r = Reader.create ~size:64 server in
          let got = Buffer.create size in
          let rec loop () =
            let n = 1 + Random.State.int rnd 200 in
            let timeout = Span.us (1 + Random.State.int rnd 2000) in
            match
              Fiber.with_timeout Mono timeout (fun () -> Reader.ensure r n)
            with
            | () ->
                Buffer.add_string got (Reader.sub_string r 0 n) ;
                Reader.consume r n ;
                loop ()
            | exception Fiber.Timeout ->
                loop ()
            | exception End_of_file ->
                let rest = Reader.buffered r in
                Buffer.add_string got (Reader.sub_string r 0 rest) ;
                Reader.consume r rest
          in
          loop () ;
          (Buffer.contents got, data) ) )

let tests =
  group "The read rule"
    [ test "a Timeout keeps the plaintext and a part of a record" (fun () ->
          with_raw (fun server tcp tls ->
              let tls = send tcp tls "abcdef" in
              equal string "ab" (read_string server 2) ;
              (* The deadline applies only to a wait for the TCP flow. *)
              Flow.set_read_deadline server (Some (Time.now Mono)) ;
              equal string "cdef" (read_string server 16) ;
              raises_timeout (fun () -> ignore (read_string server 16)) ;
              (* Half of a record, a Timeout, then the rest of the record. *)
              let tls, out = encrypt tls "ghij" in
              let half = String.length out / 2 in
              Flow.write_string tcp (String.sub out 0 half) ;
              Time.sleep Mono (Span.ms 10) ;
              raises_timeout (fun () -> ignore (read_string server 16)) ;
              equal (option int) None
                (Flow.read_now server (Bytes.create 4) 0 4) ;
              Flow.write_string tcp
                (String.sub out half (String.length out - half)) ;
              Time.sleep Mono (Span.ms 10) ;
              (* The rest came: one read_now gives the record. *)
              let buf = Bytes.create 16 in
              equal (option int) (Some 4) (Flow.read_now server buf 0 16) ;
              equal string "ghij" (Bytes.sub_string buf 0 4) ;
              Flow.set_read_deadline server None ;
              ignore (send tcp tls "k") ;
              equal string "k" (read_string server 16) ) )
    ; test "records in small TCP segments" (fun () ->
          (* Two records of 3000 bytes in TCP writes of 7 bytes: the carry of
             the session holds the parts until each record is complete. *)
          with_raw (fun server tcp tls ->
              let a = String.make 3000 'a' and b = String.make 3000 'b' in
              let tls, ra = encrypt tls a in
              let _, rb = encrypt tls b in
              let wire = ra ^ rb in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      let rec go i =
                        if i < String.length wire then (
                          let n = min 7 (String.length wire - i) in
                          Flow.write_string tcp (String.sub wire i n) ;
                          Time.sleep Mono (Span.us 10) ;
                          go (i + n) )
                      in
                      go 0 ) ;
                  equal string (a ^ b) (read_n server 6000) ) ) )
    ; test "the plaintext before a failure, then Closed each time" (fun () ->
          with_raw (fun server tcp tls ->
              let _, out = encrypt tls "abcdef" in
              Flow.write_string tcp (out ^ bad_record) ;
              Time.sleep Mono (Span.ms 10) ;
              equal string "ab" (read_string server 2) ;
              equal (option int) (Some 4)
                (Flow.read_now server (Bytes.create 16) 0 16) ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              raises_closed (fun () -> ignore (read_string server 16)) ;
              (* No close_notify can go on a failed session, but the end of the
                 TCP stream goes. *)
              Flow.shutdown_send server ;
              equal string "" (read_all tcp) ) )
    ; test "a cancel between the TCP read and the session" (fun () ->
          (* The base flow of the server cancels the scope of the reader after
             it got the bytes of the record. Thus the cancel comes at the next
             cancellation point: the state lock of the session. *)
          let armed = ref None in
          let swrap tcp =
            (* The base flow is never readable: only the carry of the session
               makes the TLS flow readable. *)
            fake_tcp tcp ~readable:never ~read:(fun buf off len ->
                let n = Flow.read tcp buf off len in
                Option.iter
                  (fun s ->
                    armed := None ;
                    Fiber.cancel s Exit )
                  !armed ;
                n )
          in
          (* [cancelled server] reads in a scope that the base flow cancels. *)
          let cancelled server =
            match
              Fiber.scope (fun s ->
                  armed := Some s ;
                  read_string server 16 )
            with
            | x ->
                x
            | exception _ ->
                "a cancel"
          in
          pair ~swrap (fun server client _ ->
              Flow.write_string client "x" ;
              Time.sleep Mono (Span.ms 10) ;
              equal string "a cancel" (cancelled server) ;
              is_true ~msg:"readable" (ready server) ;
              (* The bytes are not in the base flow: read_now gives them to the
                 session. *)
              let buf = Bytes.create 16 in
              equal (option int) (Some 1) (Flow.read_now server buf 0 16) ;
              equal char 'x' (Bytes.get buf 0) ;
              (* A read after a cancel. *)
              Flow.write_string client "y" ;
              Time.sleep Mono (Span.ms 10) ;
              equal string "a cancel" (cancelled server) ;
              equal string "y" (read_string server 16) ) )
    ; test "a cancel of a read loses no byte" (fun () ->
          List.iter
            (fun domains ->
              for seed = 1 to 20 do
                for delay = 0 to 3 do
                  equal string
                    ~msg:(Printf.sprintf "seed %d, %d domains" seed domains)
                    "x"
                    (read_after_cancel ~seed ~domains ~delay)
                done
              done )
            [1; 4] )
    ; test "a Reader with cancels gets each byte in order" (fun () ->
          List.iter
            (fun domains ->
              for seed = 1 to 10 do
                let got, sent = stream ~seed ~domains in
                is_true
                  ~msg:(Printf.sprintf "seed %d, %d domains" seed domains)
                  (String.equal got sent)
              done )
            [1; 4] ) ]
