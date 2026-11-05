(* A chat server. A client sends its name on the first line, then each line that
   it sends goes to all of the other clients. The code uses only the API of
   weft, so it runs in each world. *)

open Weft

(* The lines of a socket. A line has a maximum of 1024 bytes. *)
module Lines = struct
  type t = {c: Net.t; pending: Buffer.t; chunk: bytes}

  let create c = {c; pending= Buffer.create 128; chunk= Bytes.create 1024}

  (* The next line, with no "\n", or [None] at the end of the stream. *)
  let rec read t =
    let s = Buffer.contents t.pending in
    match String.index_opt s '\n' with
    | Some i ->
        Buffer.clear t.pending ;
        Buffer.add_substring t.pending s (i + 1) (String.length s - i - 1) ;
        Some (String.sub s 0 i)
    | None when Buffer.length t.pending > 1024 ->
        failwith "a line of more than 1024 bytes"
    | None -> (
      match Net.read t.c t.chunk 0 (Bytes.length t.chunk) with
      | 0 ->
          if s = "" then None else (Buffer.clear t.pending ; Some s)
      | n ->
          Buffer.add_subbytes t.pending t.chunk 0 n ;
          read t )
end

(* A queue with no limit. [put] never waits, thus a slow client does not stop
   the other clients. [take] waits until the queue has a value. The lock and the
   condition suspend the fiber, not the domain. *)
module Mailbox = struct
  type 'a t = {m: Fiber.Mutex.t; c: Fiber.Condition.t; q: 'a Queue.t}

  let create () =
    {m= Fiber.Mutex.create (); c= Fiber.Condition.create (); q= Queue.create ()}

  let put t v =
    Fiber.Mutex.use_rw ~protect:false t.m (fun () ->
        Queue.push v t.q ; Fiber.Condition.signal t.c )

  let take t =
    Fiber.Mutex.use_rw ~protect:false t.m (fun () ->
        while Queue.is_empty t.q do
          Fiber.Condition.wait t.c t.m
        done ;
        Queue.pop t.q )
end

type t = {m: Fiber.Mutex.t; clients: (Fiber.Id.t, string Mailbox.t) Hashtbl.t}

(* Send [line] to each client, except [from]. *)
let broadcast t ~from line =
  Fiber.Mutex.use_ro t.m (fun () ->
      Hashtbl.iter
        (fun id box -> if not (Fiber.Id.equal id from) then Mailbox.put box line)
        t.clients )

let join t box =
  Fiber.Mutex.use_rw ~protect:true t.m (fun () ->
      Hashtbl.replace t.clients (Fiber.self ()) box ;
      Hashtbl.length t.clients )

let leave t =
  Fiber.Mutex.use_rw ~protect:true t.m (fun () ->
      Hashtbl.remove t.clients (Fiber.self ()) )

(* Say "bye" to a client in a shield: without the shield, the write of a
   cancelled fiber raises [Cancelled] at once. The timeout stops a client that
   does not read. *)
let bye c =
  Fiber.shield (fun () ->
      try
        Fiber.with_timeout Mono (Span.s 1) (fun () ->
            Net.write c "server: bye\n" )
      with Fiber.Timeout | Net.Error _ -> () )

(* The lines of one client. A daemon fiber writes the lines of the mailbox, and
   this fiber reads the lines of the client. When the client ends its stream,
   the scope cancels the daemon. *)
let session t c lines name =
  let box = Mailbox.create () in
  let me = Fiber.self () in
  let n = join t box in
  (* The cleanup takes the lock, and a lock is a point where a cancel stops the
     fiber. Thus the cleanup runs in a shield. *)
  let cleanup () =
    Fiber.shield (fun () ->
        leave t ;
        broadcast t ~from:me (name ^ " left") )
  in
  Fun.protect ~finally:cleanup (fun () ->
      Net.write c (Printf.sprintf "welcome %s, %d here\n" name n) ;
      broadcast t ~from:me (name ^ " joined") ;
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              while true do
                Net.write c (Mailbox.take box ^ "\n")
              done ) ;
          let rec loop () =
            match Lines.read lines with
            | None ->
                ()
            | Some line ->
                broadcast t ~from:me (name ^ ": " ^ line) ;
                loop ()
          in
          loop () ) )

(* One client: its first line is its name. When the server stops, a cancel stops
   the session, and the client gets "bye". *)
let handle t c =
  let lines = Lines.create c in
  Fun.protect
    ~finally:(fun () -> Net.close c)
    (fun () ->
      match Lines.read lines with
      | None ->
          ()
      | Some name -> (
        try session t c lines name
        with Fiber.Cancelled _ as e -> bye c ; raise e ) )

exception Stopped

(* Accept the clients until [stop] is ready. Then cancel the handlers, which say
   "bye" to their clients, and give [()]. A handler that fails goes to
   [on_error] and does not stop the server. *)
let serve ?(stop = never)
    ?(on_error = fun e _ -> prerr_endline ("chat: " ^ Printexc.to_string e)) l =
  let t = {m= Fiber.Mutex.create (); clients= Hashtbl.create 16} in
  match
    Fiber.scope (fun s ->
        Fiber.fork_daemon s (fun () -> sync stop ; Fiber.cancel s Stopped) ;
        while true do
          let c, _ = Net.accept l in
          match Fiber.fork_isolated s ~on_error (fun () -> handle t c) with
          | () ->
              ()
          | exception Fiber.Scope_closed ->
              Net.close c
        done )
  with
  | () ->
      ()
  | exception Fiber.Cancelled Stopped ->
      ()
