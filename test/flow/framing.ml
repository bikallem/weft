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

(* WebSocket frames (RFC 6455, section 5.2): a byte with the FIN bit and the
   opcode, a byte with the MASK bit and a length of 7 bits, then a length of 16
   bits for 126, or of 64 bits for 127, big-endian, then the key of 4 bytes if
   the MASK bit is set, then the payload, masked with the key. *)
module Ws_model = struct
  type frame = {fin: bool; opcode: int; key: string option; payload: string}

  let pp_frame ppf f =
    Format.fprintf ppf "{fin %b; opcode %d; key %s; %d bytes}" f.fin f.opcode
      (match f.key with None -> "none" | Some k -> String.escaped k)
      (String.length f.payload)

  (* [mask key phase s] is [s] with each byte [i] XOR the byte [(i + phase) mod
     4] of [key]. A mask two times gives [s]. *)
  let mask key phase s =
    String.mapi
      (fun i c -> Char.chr (Char.code c lxor Char.code key.[(i + phase) land 3]))
      s

  let encode_header f =
    let b = Buffer.create 14 in
    Buffer.add_char b (Char.chr ((if f.fin then 0x80 else 0) lor f.opcode)) ;
    let m = if Option.is_some f.key then 0x80 else 0 in
    let n = String.length f.payload in
    if n <= 125 then Buffer.add_char b (Char.chr (m lor n))
    else if n <= 0xffff then begin
      Buffer.add_char b (Char.chr (m lor 126)) ;
      Buffer.add_char b (Char.chr (n lsr 8)) ;
      Buffer.add_char b (Char.chr (n land 0xff))
    end
    else begin
      Buffer.add_char b (Char.chr (m lor 127)) ;
      for i = 7 downto 0 do
        Buffer.add_char b (Char.chr ((n lsr (8 * i)) land 0xff))
      done
    end ;
    Option.iter (Buffer.add_string b) f.key ;
    Buffer.contents b

  let encode frames =
    String.concat ""
      (List.concat_map
         (fun f ->
           [ encode_header f
           ; ( match f.key with
             | None ->
                 f.payload
             | Some k ->
                 mask k 0 f.payload ) ] )
         frames )
end
