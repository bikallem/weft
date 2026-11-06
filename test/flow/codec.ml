(* The codecs of the tests on Reader and Writer, for the framings of
   framing.ml. *)

open Weft_flow
open Framing

(* A field of a packet is not in the packet: the decoder found an error. *)
exception Error of string

module Mysql = struct
  open Mysql_model

  let write_packet w p =
    Writer.start_frame w ~header:4 ;
    Writer.write_string w p.payload ;
    let n = Writer.end_frame w in
    Writer.write_uint24_le w n ; Writer.write_uint8 w p.seq

  let read_packet r =
    Reader.ensure r 4 ;
    let n = Reader.get_uint24_le r 0 and seq = Reader.get_uint8 r 3 in
    Reader.consume r 4 ;
    Reader.ensure r n ;
    let payload = Reader.sub_string r 0 n in
    Reader.consume r n ; {seq; payload}

  (* [read_packets r] reads packets to the end of the stream. *)
  let read_packets r =
    let rec loop acc =
      match read_packet r with
      | p ->
          loop (p :: acc)
      | exception End_of_file ->
          List.rev acc
    in
    loop []

  let write_field w = function
    | Int v when below v 251L ->
        Writer.write_uint8 w (Int64.to_int v)
    | Int v when below v 0x1_0000L ->
        Writer.write_uint8 w 0xfc ;
        Writer.write_uint16_le w (Int64.to_int v)
    | Int v when below v 0x100_0000L ->
        Writer.write_uint8 w 0xfd ;
        Writer.write_uint24_le w (Int64.to_int v)
    | Int v ->
        Writer.write_uint8 w 0xfe ; Writer.write_int64_le w v
    | Text s ->
        Writer.write_string w s ; Writer.write_uint8 w 0

  let write_row w seq fields =
    Writer.start_frame w ~header:4 ;
    List.iter (write_field w) fields ;
    let n = Writer.end_frame w in
    Writer.write_uint24_le w n ; Writer.write_uint8 w seq

  (* [need off k limit] makes sure that [k] bytes at [off] are in the packet,
     which ends at [limit]. The reader can hold the next packet after it, thus
     its own checks are not sufficient. *)
  let need off k limit = if off + k > limit then raise (Error "truncated")

  (* [read_field r off limit] reads the field at [off] of a packet that ends at
     [limit], and gives it with the offset after it. A field is a text when
     [text] is [true]. *)
  let read_field r ~text off limit =
    if text then begin
      let start = Reader.view r off (limit - off) in
      let buf = Reader.buffer r in
      match Bytes.index_from_opt buf start '\000' with
      | Some i when i < start + (limit - off) ->
          (Text (Bytes.sub_string buf start (i - start)), off + (i - start) + 1)
      | _ ->
          raise (Error "no NUL")
    end
    else begin
      need off 1 limit ;
      match Reader.get_uint8 r off with
      | b when b < 0xfb ->
          (Int (Int64.of_int b), off + 1)
      | 0xfc ->
          need off 3 limit ;
          (Int (Int64.of_int (Reader.get_uint16_le r (off + 1))), off + 3)
      | 0xfd ->
          need off 4 limit ;
          (Int (Int64.of_int (Reader.get_uint24_le r (off + 1))), off + 4)
      | 0xfe ->
          need off 9 limit ;
          (Int (Reader.get_int64_le r (off + 1)), off + 9)
      | b ->
          raise (Error (Printf.sprintf "prefix %d" b))
    end

  (* [read_row r kinds] reads a packet with the fields of [kinds] ([true] for a
     text). Fields after the end of the packet raise [Codec]. *)
  let read_row r kinds =
    Reader.ensure r 4 ;
    let n = Reader.get_uint24_le r 0 in
    Reader.consume r 4 ;
    Reader.ensure r n ;
    let fields, at =
      List.fold_left
        (fun (acc, off) text ->
          let f, off = read_field r ~text off n in
          (f :: acc, off) )
        ([], 0) kinds
    in
    if at <> n then raise (Error "bytes after the fields") ;
    Reader.consume r n ;
    List.rev fields
end
