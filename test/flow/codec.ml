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

module Ws = struct
  open Ws_model

  let write_frame w f =
    Writer.write_uint8 w ((if f.fin then 0x80 else 0) lor f.opcode) ;
    let m = if Option.is_some f.key then 0x80 else 0 in
    let n = String.length f.payload in
    if n <= 125 then Writer.write_uint8 w (m lor n)
    else if n <= 0xffff then begin
      Writer.write_uint8 w (m lor 126) ;
      Writer.write_uint16_be w n
    end
    else begin
      Writer.write_uint8 w (m lor 127) ;
      Writer.write_int64_be w (Int64.of_int n)
    end ;
    match f.key with
    | None ->
        Writer.write_string w f.payload
    | Some k ->
        Writer.write_string w k ;
        Writer.write_string w (mask k 0 f.payload)

  (* The payload comes in parts with read_into, unmasked with the phase of each
     part, as caravan reads it. *)
  let read_frame r =
    Reader.ensure r 2 ;
    let b0 = Reader.get_uint8 r 0 and b1 = Reader.get_uint8 r 1 in
    Reader.consume r 2 ;
    let n =
      match b1 land 0x7f with
      | 126 ->
          Reader.ensure r 2 ;
          let n = Reader.get_uint16_be r 0 in
          Reader.consume r 2 ; n
      | 127 ->
          Reader.ensure r 8 ;
          let n = Int64.to_int (Reader.get_int64_be r 0) in
          Reader.consume r 8 ; n
      | n ->
          n
    in
    let key =
      if b1 land 0x80 = 0 then None
      else begin
        Reader.ensure r 4 ;
        let k = Reader.sub_string r 0 4 in
        Reader.consume r 4 ; Some k
      end
    in
    let buf = Bytes.create n in
    let rec loop pos =
      if pos < n then begin
        let k = Reader.read_into r buf pos (n - pos) in
        if k = 0 then raise End_of_file ;
        Option.iter
          (fun key ->
            Bytes.blit_string
              (mask key (pos land 3) (Bytes.sub_string buf pos k))
              0 buf pos k )
          key ;
        loop (pos + k)
      end
    in
    loop 0 ;
    { fin= b0 land 0x80 <> 0
    ; opcode= b0 land 0x0f
    ; key
    ; payload= Bytes.unsafe_to_string buf }

  let read_frames r =
    let rec loop acc =
      match Reader.ensure r 1 with
      | () ->
          loop (read_frame r :: acc)
      | exception End_of_file ->
          List.rev acc
    in
    loop []
end
