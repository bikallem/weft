(* The models of the framings of the users of Reader and Writer, for the tests:
   MySQL packets, and WebSocket frames. They use only the stdlib, with no Reader
   and no Writer. The tests compare the codecs of codec.ml with them. *)

(* MySQL packets: a length of 3 bytes, little-endian, a sequence id of 1 byte,
   then the payload. The fields of a row are length-encoded integers and strings
   that end with a NUL byte. *)
module Mysql_model = struct
  type packet = {seq: int; payload: string}

  let pp_packet ppf p = Format.fprintf ppf "{seq %d; %S}" p.seq p.payload

  let add_packet b p =
    let n = String.length p.payload in
    Buffer.add_char b (Char.chr (n land 0xff)) ;
    Buffer.add_char b (Char.chr ((n lsr 8) land 0xff)) ;
    Buffer.add_char b (Char.chr (n lsr 16)) ;
    Buffer.add_char b (Char.chr p.seq) ;
    Buffer.add_string b p.payload

  let encode packets =
    let b = Buffer.create 64 in
    List.iter (add_packet b) packets ;
    Buffer.contents b

  (* [decode s] is the complete packets at the start of [s], and the number of
     their bytes. *)
  let decode s =
    let rec loop at acc =
      if at + 4 > String.length s then (List.rev acc, at)
      else
        let n =
          Char.code s.[at]
          lor (Char.code s.[at + 1] lsl 8)
          lor (Char.code s.[at + 2] lsl 16)
        in
        if at + 4 + n > String.length s then (List.rev acc, at)
        else
          loop
            (at + 4 + n)
            ( {seq= Char.code s.[at + 3]; payload= String.sub s (at + 4) n}
            :: acc )
    in
    loop 0 []

  (* A field of a row. An integer is unsigned: a negative [int64] is a value of
     2^63 or more. *)
  type field = Int of int64 | Text of string

  let pp_field ppf = function
    | Int v ->
        Format.fprintf ppf "Int %Lu" v
    | Text s ->
        Format.fprintf ppf "Text %S" s

  (* [le b v n] adds the [n] low bytes of [v], little-endian. *)
  let le b v n =
    for i = 0 to n - 1 do
      Buffer.add_char b
        (Char.chr
           (Int64.to_int
              (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xffL) ) )
    done

  let below v limit = Int64.unsigned_compare v limit < 0

  let add_field b = function
    | Int v when below v 251L ->
        le b v 1
    | Int v when below v 0x1_0000L ->
        Buffer.add_char b '\xfc' ; le b v 2
    | Int v when below v 0x100_0000L ->
        Buffer.add_char b '\xfd' ; le b v 3
    | Int v ->
        Buffer.add_char b '\xfe' ; le b v 8
    | Text s ->
        Buffer.add_string b s ; Buffer.add_char b '\000'

  let row fields =
    let b = Buffer.create 32 in
    List.iter (add_field b) fields ;
    Buffer.contents b
end
