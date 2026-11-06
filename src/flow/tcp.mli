(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** TCP and Unix stream connections as {!Flow.t} values, and listening sockets.

    All waits are weft events. A read, a write, a connect or an accept that
    waits suspends only its fiber. *)

val flow : Weft.Net.t -> Flow.t
(** [flow s] is the flow of the connected socket [s]. The flow owns [s]:
    {!Flow.close} closes it.
    - Reads and writes wait for the socket when it is not ready. A read or a
      write that waits raises {!Flow.Timeout} at the earlier of the deadline of
      the flow ({!Flow.set_read_deadline}) and the deadline of the call. The
      deadline of the call is the time of its first wait plus the timeout of the
      flow ({!Flow.set_read_timeout}). The deadlines are on the [Mono] line of
      [Weft.Time].
    - A reset or a broken connection raises {!Flow.Closed}. Other errors of the
      socket raise [Weft.Net.Error].
    - Each read and write is first a cancellation point
      ([Weft.Fiber.check_cancel]): if the fiber is cancelled, it raises
      [Weft.Fiber.Cancelled]. Without this, a read of a socket that is always
      ready never sees the cancel, because a weft read with data does not sync.
    - {!Flow.shutdown_send} ends the send side, and {!Flow.close} closes the
      socket. A read or a write that waits in another fiber then raises
      {!Flow.Closed}. weft closes the fd only when no operation uses it.
    - After a {!Flow.Timeout}, a cancel or an error, a write can have sent a
      part of its bytes. The state of the stream is then not known: close the
      flow.

    The functions of the flow need a weft fiber. *)

val connect : Weft.Net.addr -> Flow.t
(** [connect addr] connects to [addr] and gives the flow of the connection. A
    TCP connection has [TCP_NODELAY]. The connect has no deadline: put
    [Weft.Fiber.with_timeout] around it. On each exception, also a cancel, no
    socket stays open.

    @raise Weft.Net.Error if the connect gives an error, for example [Refused].
*)

(** Listening sockets. *)
module Listener : sig
  (** The type for listening sockets. *)
  type t

  val listen : ?backlog:int -> Weft.Net.addr -> t
  (** [listen ~backlog addr] listens on [addr], with a queue of [backlog]
      connections (default: that of [Weft.Net.listen]). A TCP listener has
      [SO_REUSEADDR]. Listen on port [0] to get a free port, see {!addr}.

      @raise Weft.Net.Error if the address cannot be used, for example [In_use].
  *)

  val addr : t -> Weft.Net.addr
  (** [addr l] is the address of [l], with its port. It is the value at the
      listen, so it stays the same after {!close}. *)

  val accept : t -> Weft.Net.t * Weft.Net.addr
  (** [accept l] waits for a connection on [l], and gives its socket and the
      address of the client. The socket of a TCP connection has [TCP_NODELAY].
      The wait is a cancellation point. The caller owns the socket. No sync
      occurs between the accept of the socket and the return, so a cancel cannot
      lose the socket. Take the ownership with no sync between, for example with
      a [Fun.protect] at once.

      @raise Weft.Net.Error
        with [Bad_fd] if [l] is closed, also by another fiber during the wait.
        It raises [No_resources _] if the process has no free fd (the connection
        stays in the queue, see [Weft.Net.accept]), and the other errors of
        accept(2). *)

  val close : t -> unit
  (** [close l] stops the listen. A waiting {!accept} raises
      [Weft.Net.Error (Bad_fd, _)]. The connections in the queue get a reset. A
      second close does nothing. It never waits. *)
end
