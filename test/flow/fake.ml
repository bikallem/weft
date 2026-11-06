(* A flow in memory that logs its calls, for the tests of Reader. Thus a test
   sees the size of each read and write, and the bytes that the flow got, with
   no function of the API for the tests. It needs no fiber. *)

open Weft_flow

(* One call of the read or the write of the flow: its arguments. [buf] is the
   bytes that the flow got, for a check of physical equality. *)
type call = {buf: bytes; off: int; len: int}

type t =
  { flow: Flow.t
  ; reads: call list ref (* The reads, the last first. *)
  ; writes: call list ref (* The writes, the last first. *)
  ; output: Buffer.t (* The bytes of the writes that did not raise. *) }

(* [make ~chunk ~write_raises input] is a flow that reads [input], at most
   [chunk] bytes in each read (default: no limit), then gives 0. A write logs
   its call, then raises [write_raises] if it is given. If not, it adds the
   bytes to [output]. *)
let make ?(chunk = max_int) ?write_raises input =
  let reads = ref [] and writes = ref [] and output = Buffer.create 64 in
  let pos = ref 0 in
  let read buf off len =
    reads := {buf; off; len} :: !reads ;
    let n = Int.min (Int.min len chunk) (String.length input - !pos) in
    Bytes.blit_string input !pos buf off n ;
    pos := !pos + n ;
    n
  in
  let write buf off len =
    writes := {buf; off; len} :: !writes ;
    match write_raises with
    | Some e ->
        raise e
    | None ->
        Buffer.add_subbytes output buf off len
  in
  let flow =
    Flow.make ~read
      ~read_now:(fun buf off len -> Some (read buf off len))
      ~write ~readable:(Weft.always ()) ~shutdown_send:ignore ~close:ignore
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  {flow; reads; writes; output}

(* The [len] of each read, in order. *)
let read_lens f = List.rev_map (fun c -> c.len) !(f.reads)

(* The [len] of each write, in order. *)
let write_lens f = List.rev_map (fun c -> c.len) !(f.writes)

(* The input [0123456789abcdefghij...] of [n] bytes: the byte [i] is the
   character [i] of a cycle of 36 characters. A test sees the place of a byte
   from its value. *)
let digits n =
  String.init n (fun i -> "0123456789abcdefghijklmnopqrstuvwxyz".[i mod 36])
