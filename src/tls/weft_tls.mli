(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(** TLS flows, with ocaml-tls.

    {[
    let () = Mirage_crypto_rng_unix.use_default ()

    let ok = function Ok v -> v | Error (`Msg m) -> failwith m

    let config =
      ok
        (Tls.Config.client
           ~authenticator:(ok (Ca_certs.authenticator ()))
           ~peer_name:Domain_name.(host_exn (of_string_exn "example.com"))
           () )

    let main () =
      let tcp = Weft_flow.Tcp.connect (Weft.Net.Tcp ("example.com", 443)) in
      let flow, _ = Weft_tls.client config tcp in
      Weft_flow.Flow.write_string flow "hello"
    ]}

    {1:rng The random number generator}

    ocaml-tls takes its randomness from [Mirage_crypto_rng.default_generator].
    Set it one time, before the first handshake, with
    [Mirage_crypto_rng_unix.use_default ()]. That generator calls the OS for
    each request and keeps no state, thus it is safe on many domains. Do not use
    a Fortuna generator on more than one domain: it is not safe there.

    {1:flow The TLS flow}

    {!server} and {!client} give a {!Weft_flow.Flow.t}. A read and a write can
    run at the same time in two fibers, as the flow contract permits. A reader
    never waits for a write of the base flow: it waits for a writer only while
    the writer encrypts one record. Thus [Weft_flow.Flow.read_now] never waits
    for input, but it can wait for the lock of the session, and a cancel can
    come in it. A response of the session to the peer, for example a TLS 1.3
    KeyUpdate, goes out before the next write.

    A read that raises took no byte of the stream, as {!Weft_flow.Flow.make}
    requires. After a cancel, a [Weft_flow.Flow.Timeout] or a
    [Weft_flow.Flow.Closed] in a read, the plaintext that the session decrypted
    stays in the flow, and a later read gives it. This is also true for the
    bytes of the base flow that the session did not decrypt before a cancel.
    Thus a {!Weft_flow.Reader} on the TLS flow gives each byte in order.

    After the handshake:
    - A TLS failure of the peer (for example a bad record or an alert) makes a
      read or a write raise [Weft_flow.Flow.Closed], after the plaintext that
      came before it. The flow sends no alert.
    - The end of the stream of the base flow with no [close_notify] makes a read
      raise [Weft_flow.Flow.Closed]: it can be a truncation. A [close_notify] of
      the peer gives the end of the stream (a read gives 0).
    - A session permits 64 small records with no plaintext (for example
      KeyUpdate messages), and one more for each 16 KiB of plaintext. A record
      with no plaintext costs one more for each KiB of its size. More fail the
      flow: ocaml-tls 2.1.3 keeps memory for each KeyUpdate, and for a part of a
      handshake message.
    - After a [close_notify] of the peer, the flow ignores the records that
      follow it.
    - [Weft_flow.Flow.shutdown_send] sends [close_notify], then the end of the
      stream of the base flow. [Weft_flow.Flow.close] sends nothing: it never
      waits.
    - A write sends one record for each 16383 bytes. A write that raises can
      send a part of its bytes: a cancel between two records leaves the records
      before it sent, and the flow open. A write of the base flow that raises,
      also for a cancel, can send a part of a record. Then the flow is failed,
      and each later write raises [Weft_flow.Flow.Closed]. A read then gives the
      plaintext that the session decrypted before the failure, and then raises
      [Weft_flow.Flow.Closed], also after a [close_notify] of the peer. The
      session does not decrypt more records.

    {1:handshake The handshake}

    The handshake reads with [Weft_flow.Flow.read] of the base flow. Thus the
    read deadline and the read timeout of the base flow apply at each wait. A
    read of a {!Weft_flow.Tcp} flow is a cancellation point, thus a cancel, for
    example of [Weft.Fiber.with_timeout], ends the handshake at its next read. A
    read with bytes that are ready does not examine the deadline. Thus a peer
    that keeps the socket full can delay the deadline until the handshake has 64
    KiB: then the handshake fails.

    A read timeout ({!Weft_flow.Flow.set_read_timeout}) applies to each read,
    thus a peer that sends one byte before each timeout keeps the handshake for
    up to 64 KiB of reads. To limit the time of all of the handshake, set a read
    deadline ({!Weft_flow.Flow.set_read_deadline}) before it, or run it in
    [Weft.Fiber.with_timeout].

    {1:upgrade TLS in the middle of a protocol}

    {!client} and {!server} can start on a flow that already carried bytes in
    clear text, for example after the SSL request of MySQL or a STARTTLS
    command. The TLS flow reads only from the base flow. Thus the caller must
    make sure that no byte of the peer stays buffered above the base flow, for
    example in a {!Weft_flow.Reader}. Never read such bytes as data of the
    protocol after the upgrade: an attacker can put clear text there, and the
    protocol then reads it as if it came over TLS. Do one of these:
    - If the peer must wait for an answer before its handshake (a MySQL client
      after the greeting, a STARTTLS server after the command), a held byte is a
      sign of an attack. Make sure that [Weft_flow.Reader.buffered r = 0], and
      close the connection if not.
    - If the peer can send its handshake with no wait after its request (a MySQL
      server after the SSL request), the reader can hold the start of the
      handshake. Give the held bytes to the handshake with
      {!Weft_flow.Flow.prepend}, for example
      [Weft_tls.server config (Flow.prepend (Reader.sub_string r 0 n) flow)]
      with [n = Reader.buffered r]. The TLS session authenticates these bytes,
      or the handshake fails. *)

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
    TLS flow and the data of the session, for example its ALPN protocol (see
    {!handshake}).

    @raise Invalid_argument
      if no default generator is set (see {!rng}). [server] then reads and
      writes no byte.
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
    name [peer_name] or the address [ip] of [config] (see [Tls.Config.client],
    [Tls.Config.peer] and [Tls.Config.ip]). Without a peer name or an address,
    the authenticator checks no name: give one for each server on the Internet.

    @raise Invalid_argument
      if no default generator is set (see {!rng}). [client] then writes no byte.
*)
