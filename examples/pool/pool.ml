(* A pool of workers, and a rate limit. The code uses only the API of weft, so
   it runs in each world. *)

open Weft

(* A request to a worker. [gone] is the nack of the call: it is ready when the
   caller does not wait for the reply any more. *)
type ('a, 'b) request =
  {arg: 'a; reply: ('b, exn) result Ivar.t; gone: unit event}

type ('a, 'b) t = {scope: Fiber.scope; requests: ('a, 'b) request Chan.t}

(* Do one request. The job runs in a promise, and the worker waits for the first
   of two events: the end of the job, or the nack. If the caller left, the
   worker cancels the job and takes the next request. *)
let serve f r =
  Fiber.scope (fun s ->
      let p = Fiber.fork_promise s (fun () -> f r.arg) in
      match
        sync
          (choose
             [ wrap r.gone (fun () -> `Gone)
             ; wrap (Promise.ready p) (fun v -> `Done v) ] )
      with
      | `Done v ->
          Ivar.fill r.reply (Ok v)
      | `Gone ->
          Promise.cancel p
      | exception (Fiber.Cancelled _ as e) ->
          raise e
      | exception e ->
          Ivar.fill r.reply (Error e) )

(* Start [workers] fibers in the scope [s] that apply [f] to the requests. The
   workers are daemons: the scope cancels them when its other fibers end. *)
let create s ~workers f =
  let requests = Chan.create () in
  for i = 1 to workers do
    Fiber.fork_daemon ~name:(Printf.sprintf "worker-%d" i) s (fun () ->
        while true do
          serve f (sync (Chan.recv requests))
        done )
  done ;
  {scope= s; requests}

(* The event of a call of the pool. This is the RPC pattern of Concurrent ML.
   [with_nack] gives the nack event, and [guard] makes the request at sync time.
   Thus each sync makes a new request.

   A fiber of the pool sends the request. Thus the sync does not wait for a free
   worker before it can select another branch.

   If the sync selects another branch, for example a timeout, or if a cancel
   stops it, the nack becomes ready. Then the send stops, or the worker cancels
   the job. *)
let call_ev t arg =
  with_nack (fun gone ->
      guard (fun () ->
          let reply = Ivar.create () in
          let r = {arg; reply; gone} in
          Fiber.fork t.scope (fun () ->
              sync (choose [Chan.send t.requests r; gone]) ) ;
          wrap (Ivar.read reply) (function Ok v -> v | Error e -> raise e) ) )

let call t arg = sync (call_ev t arg)

(* A call with a timeout: [None] if no reply comes in time. *)
let call_timeout t span arg =
  sync
    (choose
       [ wrap (call_ev t arg) Option.some
       ; wrap (Time.after Mono span) (fun () -> None) ] )

(* A rate limit: a bucket of tokens. A daemon fiber adds one token at each
   period, up to [burst] tokens. [Semaphore.try_release] gives [false] when the
   bucket is full, and that is normal. *)
module Rate = struct
  type t = Fiber.Semaphore.t

  let create s ~per_s ~burst =
    let tokens = Fiber.Semaphore.create ~max:burst 0 in
    let period = Span.of_float_s (1. /. float_of_int per_s) in
    Fiber.fork_daemon ~name:"rate" s (fun () ->
        while true do
          Time.sleep Mono period ;
          ignore (Fiber.Semaphore.try_release tokens)
        done ) ;
    tokens

  (* Take a token. The acquire is an event, thus a caller can also use it in a
     [choose], for example with a timeout. *)
  let take t = sync (Fiber.Semaphore.acquire t)
end
