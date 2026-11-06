(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** TLS flows, with ocaml-tls.

    {1:rng The random number generator}

    ocaml-tls takes its randomness from [Mirage_crypto_rng.default_generator].
    Set it one time, before the first handshake, with
    [Mirage_crypto_rng_unix.use_default ()]. That generator calls the OS for
    each request and keeps no state, thus it is safe on many domains. Do not use
    a Fortuna generator on more than one domain: it is not safe there.

    {1:flow The TLS flow}

    {!server} and {!client} give a {!Weft_flow.Flow.t}. A read and a write can
    run at the same time in two fibers, as the flow contract permits. A reader
    never waits for a TCP write: it waits for a writer only while the writer
    encrypts one record. Thus [Weft_flow.Flow.read_now] never waits for input,
    but it can wait for the lock of the session, and a cancel can come in it. A
    response of the session to the peer, for example a TLS 1.3 KeyUpdate, goes
    out before the next write.

    After the handshake:
    - A TLS failure of the peer (for example a bad record or an alert) makes a
      read or a write raise [Weft_flow.Flow.Closed], after the plaintext that
      came before it. The flow sends no alert.
    - The end of the TCP stream with no [close_notify] makes a read raise
      [Weft_flow.Flow.Closed]: it can be a truncation. A [close_notify] of the
      peer gives the end of the stream (a read gives 0).
    - A session permits 64 small records with no plaintext (for example
      KeyUpdate messages), and one more for each 16 KiB of plaintext. A record
      with no plaintext costs one more for each KiB of its size. More fail the
      flow: ocaml-tls 2.1.3 keeps memory for each KeyUpdate, and for a part of a
      handshake message.
    - After a [close_notify] of the peer, the flow ignores the records that
      follow it.
    - [Weft_flow.Flow.shutdown_send] sends [close_notify], then the end of the
      TCP stream. [Weft_flow.Flow.close] sends nothing: it never waits.
    - A write that raises, also for a cancel, can send a part of a record. Then
      the flow is failed, and each later write raises [Weft_flow.Flow.Closed].
*)

(** A failure of the handshake: a TLS alert, a bad message, a failed check of a
    certificate, a handshake of more than 64 KiB, the end of the connection, or
    too many records with no plaintext in the same read as the end of the
    handshake (see {!flow}). The text tells the cause. *)
exception Handshake_error of string

val server :
     Tls.Config.server
  -> Weft_flow.Flow.t
  -> Weft_flow.Flow.t * Tls.Core.epoch_data
(** [server config flow] does the handshake of a server on [flow], and gives the
    TLS flow and the data of the session, for example its ALPN protocol. The
    read deadlines and timeouts of [flow] apply to the handshake.

    @raise Handshake_error
      if the handshake fails. For a failure of TLS, the alert goes to the peer
      if the connection is open.
    @raise Weft_flow.Flow.Closed
    @raise Weft_flow.Flow.Timeout as a read or a write of [flow]. *)

val client :
     Tls.Config.client
  -> Weft_flow.Flow.t
  -> Weft_flow.Flow.t * Tls.Core.epoch_data
(** [client config flow] does the handshake of a client on [flow], as {!server}.
    The authenticator of [config] checks the certificate of the server, and the
    name [peer_name] of [config] (see [Tls.Config.client] and
    [Tls.Config.peer]). Without a peer name, the authenticator checks no name:
    give one for each server on the Internet. *)
