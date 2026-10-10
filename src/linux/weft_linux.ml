open Weft.Private

external mono_ns : unit -> int64 = "weft_mono_ns"

external posix_ns : unit -> int64 = "weft_posix_ns"

external epoll_create : unit -> int = "weft_epoll_create"

external eventfd : unit -> int = "weft_eventfd"

external timerfd : unit -> int = "weft_timerfd"

external timerfd_set : int -> int64 -> int = "weft_timerfd_set"

external drain : int -> int = "weft_drain"

external eventfd_signal : int -> int = "weft_eventfd_signal"

external epoll_add : int -> int -> int64 -> int = "weft_epoll_add"

external epoll_del : int -> int -> int = "weft_epoll_del"

external epoll_wait : int -> bool -> (int64 * bool * bool) array
  = "weft_epoll_wait"

external listen : int -> string -> int -> int -> int = "weft_listen"

external local_port_of : int -> int = "weft_local_port"

external connect : int -> string -> int -> int = "weft_connect"

external connect_result : int -> int = "weft_connect_result"

external accept : int -> int * string * int = "weft_accept"

external recv : int -> bytes -> int -> int -> int = "weft_recv"

external send : int -> bytes -> int -> int -> int = "weft_send"

external shutdown_send : int -> int = "weft_shutdown_send"

external close_fd : int -> int = "weft_close"

external getrandom : bytes -> int = "weft_getrandom"

external errno_codes : unit -> int * int * int * int * int * int * int
  = "weft_errno_codes"

external strerror : int -> string = "weft_strerror"

let eagain, ebadf, econnrefused, econnreset, epipe, eaddrinuse, ewouldblock =
  errno_codes ()

let error_of e =
  if
    ( e = eagain || e = ewouldblock )
    [@mutate off "EAGAIN = EWOULDBLOCK on Linux"]
  then Op.Would_block
  else if e = ebadf then Op.Bad_fd
  else if e = econnrefused then Op.Refused
  else if e = econnreset then Op.Reset
  else if e = epipe then Op.Broken_pipe
  else if e = eaddrinuse then Op.In_use
  else Op.Other (strerror e)

let result r = if r >= 0 then Ok r else Error (error_of (-r))

let unit r = Result.map ignore (result r)

(* The tokens of the alarm and of the wake fd. A slot of an fd is never so
   large. *)
let timer_token = -1L

let wake_token = -2L

type dom =
  { epfd: int
  ; evfd: int
  ; tfd: int
  ; mutable armed: int64  (** The time of the alarm, or 0. *) }

type t = {doms: dom array; rng: Random.State.t}

let check name r = if r < 0 then failwith (name ^ ": " ^ strerror (-r)) else r

let make_dom () =
  let epfd = check "epoll_create1" (epoll_create ()) in
  let evfd = check "eventfd" (eventfd ()) in
  let tfd = check "timerfd_create" (timerfd ()) in
  ignore (check "epoll_ctl" (epoll_add epfd evfd wake_token)) ;
  ignore (check "epoll_ctl" (epoll_add epfd tfd timer_token)) ;
  {epfd; evfd; tfd; armed= 0L}

let create () =
  let seed = Bytes.create 8 in
  ignore (getrandom seed) ;
  { doms= [|make_dom ()|]
  ; rng= Random.State.make [|Int64.to_int (Bytes.get_int64_le seed 0)|] }

let close t =
  Array.iter
    (fun d ->
      ignore (close_fd d.tfd) ;
      ignore (close_fd d.evfd) ;
      ignore (close_fd d.epfd) )
    t.doms

module World = struct
  type nonrec t = t

  let now (type time) _ (line : time Weft.Clock.line) : time =
    Weft.Clock.of_ns line
      (match line with Mono -> mono_ns () | Posix -> posix_ns ())

  let host_port = function
    | Op.Tcp (h, p) ->
        (0, h, p)
    | Op.Unix path ->
        (1, path, 0)

  let submit : type a. t -> a Op.t -> (a, Op.error) result =
   fun t op ->
    match op with
    | Op.Listen (addr, backlog) ->
        let kind, h, p = host_port addr in
        result (listen kind h p backlog)
    | Op.Accept fd ->
        let r, host, port = accept fd in
        Result.map (fun fd -> (fd, Op.Tcp (host, port))) (result r)
    | Op.Connect addr ->
        let kind, h, p = host_port addr in
        result (connect kind h p)
    | Op.Connect_result fd ->
        unit (connect_result fd)
    | Op.Read (fd, buf, off, len) ->
        result (recv fd buf off len)
    | Op.Write (fd, buf, off, len) ->
        result (send fd buf off len)
    | Op.Shutdown fd ->
        unit (shutdown_send fd)
    | Op.Close fd ->
        unit (close_fd fd)
    | Op.Register (d, fd, token) ->
        unit (epoll_add t.doms.(d).epfd fd token)
    | Op.Deregister (d, fd) ->
        unit (epoll_del t.doms.(d).epfd fd)

  (* Set the alarm only when the earliest deadline changed. *)
  let arm d (deadline : Weft.Clock.mono option) =
    let target =
      match deadline with
      | None ->
          0L
      | Some m ->
          Int64.max 1L (Weft.Clock.to_ns Mono m)
    in
    if not (Int64.equal target d.armed) then (
      ignore (timerfd_set d.tfd target) ;
      d.armed <- target )

  let wait t ~domain (w : World.Wait.t) =
    let d = t.doms.(domain) in
    arm d w.deadline ;
    let events = epoll_wait d.epfd (w.timeout = World.Wait.Sleep) in
    Array.to_list events
    |> List.map (fun (token, read, write) ->
        if Int64.equal token timer_token then (
          ignore (drain d.tfd) ;
          (* A timerfd that fired is not armed. *)
          d.armed <- 0L ;
          World.Ready.Alarm )
        else if Int64.equal token wake_token then (
          ignore (drain d.evfd) ;
          World.Ready.Woken )
        else World.Ready.Fd {token; read; write} )

  let wake t ~domain = ignore (eventfd_signal t.doms.(domain).evfd)

  let random t n = Random.State.int t.rng n

  let random_bytes _ b = ignore (getrandom b)
end

let run ?slack main =
  let t = create () in
  Fun.protect
    ~finally:(fun () -> close t)
    (fun () -> Weft.run ?slack (module World) t main)

let local_port (fd : Weft.Net.t) = local_port_of fd.Sched.os
