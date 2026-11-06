(* A model of Weft_flow.Flow for dscheck: a record of the functions of
   Flow.make. *)

module Flow = struct
  exception Closed

  exception Timeout

  type t =
    { read: bytes -> int -> int -> int
    ; read_now: bytes -> int -> int -> int option
    ; write: bytes -> int -> int -> unit
    ; readable: unit Weft.event
    ; shutdown_send: unit -> unit
    ; close: unit -> unit }

  let make ~read ~read_now ~write ~readable ~shutdown_send ~close ?reset:_
      ~set_read_deadline:_ ~set_write_deadline:_ ?set_read_timeout:_
      ?set_write_timeout:_ () =
    {read; read_now; write; readable; shutdown_send; close}

  let read t = t.read

  let read_now t = t.read_now

  let write t = t.write

  let write_string t s = t.write (Bytes.unsafe_of_string s) 0 (String.length s)

  let readable t = t.readable

  let shutdown_send t = t.shutdown_send ()

  let close t = t.close ()

  let reset t = t.close ()

  let set_read_deadline _ _ = ()

  let set_write_deadline _ _ = ()

  let set_read_timeout _ _ = ()

  let set_write_timeout _ _ = ()
end
