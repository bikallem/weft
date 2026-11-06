(* The tests of Writer (design section 2.18): the unit tests on the fake flow of
   fake.ml, a property, and a stateful test against a model. They need no
   fiber. *)

open Windtrap
open Weft_flow

let is_invalid_arg = Helpers.is_invalid_arg

let output f = Buffer.contents f.Fake.output

(* A MySQL packet: [encode] gives its bytes, and [write] writes it with a
   frame. *)
let encode seq payload =
  Framing.Mysql_model.encode [{Framing.Mysql_model.seq; payload}]

let write w seq payload =
  Codec.Mysql.write_packet w {Framing.Mysql_model.seq; payload}

(* The writes of the property: one constructor for each function of the
   writer. *)
type op =
  | U8 of int
  | U16_le of int
  | U16_be of int
  | U24_le of int
  | I32_le of int32
  | I64_le of int64
  | I64_be of int64
  | Str of string

let pp_op ppf = function
  | U8 v ->
      Format.fprintf ppf "U8 %d" v
  | U16_le v ->
      Format.fprintf ppf "U16_le %d" v
  | U16_be v ->
      Format.fprintf ppf "U16_be %d" v
  | U24_le v ->
      Format.fprintf ppf "U24_le %d" v
  | I32_le v ->
      Format.fprintf ppf "I32_le %ld" v
  | I64_le v ->
      Format.fprintf ppf "I64_le %Ld" v
  | I64_be v ->
      Format.fprintf ppf "I64_be %Ld" v
  | Str s ->
      Format.fprintf ppf "Str %S" s

let gen_op =
  Gen.(
    with_pp pp_op
      (one_of
         [ map (fun v -> U8 v) (int_range 0 0xff)
         ; map (fun v -> U16_le v) (int_range 0 0xffff)
         ; map (fun v -> U16_be v) (int_range 0 0xffff)
         ; map (fun v -> U24_le v) (int_range 0 0xff_ffff)
         ; map (fun v -> I32_le v) int32
         ; map (fun v -> I64_le v) int64
         ; map (fun v -> I64_be v) int64
         ; map (fun s -> Str s) (string_of ~size:(int_range 0 40) char) ] ) )

let apply w = function
  | U8 v ->
      Writer.write_uint8 w v
  | U16_le v ->
      Writer.write_uint16_le w v
  | U16_be v ->
      Writer.write_uint16_be w v
  | U24_le v ->
      Writer.write_uint24_le w v
  | I32_le v ->
      Writer.write_int32_le w v
  | I64_le v ->
      Writer.write_int64_le w v
  | I64_be v ->
      Writer.write_int64_be w v
  | Str s ->
      Writer.write_string w s

let reference b = function
  | U8 v ->
      Buffer.add_uint8 b v
  | U16_le v ->
      Buffer.add_uint16_le b v
  | U16_be v ->
      Buffer.add_uint16_be b v
  | U24_le v ->
      Buffer.add_uint8 b (v land 0xff) ;
      Buffer.add_uint8 b ((v lsr 8) land 0xff) ;
      Buffer.add_uint8 b (v lsr 16)
  | I32_le v ->
      Buffer.add_int32_le b v
  | I64_le v ->
      Buffer.add_int64_le b v
  | I64_be v ->
      Buffer.add_int64_be b v
  | Str s ->
      Buffer.add_string b s

let unit_tests =
  group "Writer"
    [ prop "the writes give the bytes of the stdlib, for each size"
        Gen.(pair (list ~size:(int_range 0 40) gen_op) (int_range 8 64))
        (fun (ops, size) ->
          let f = Fake.make "" in
          let w = Writer.create ~size f.flow in
          let b = Buffer.create 64 in
          List.iter (fun op -> apply w op ; reference b op) ops ;
          Writer.flush w ;
          equal string (Buffer.contents b) (output f) )
    ; test "the writes refuse values out of range" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create f.flow in
          List.iter
            (fun (name, write, v) ->
              raises_match ~msg:name is_invalid_arg (fun () -> write w v) )
            [ ("uint8 256", Writer.write_uint8, 256)
            ; ("uint8 -1", Writer.write_uint8, -1)
            ; ("uint16_le 65536", Writer.write_uint16_le, 65536)
            ; ("uint16_le -1", Writer.write_uint16_le, -1)
            ; ("uint16_be 65536", Writer.write_uint16_be, 65536)
            ; ("uint16_be -1", Writer.write_uint16_be, -1)
            ; ("uint24_le 2^24", Writer.write_uint24_le, 1 lsl 24)
            ; ("uint24_le -1", Writer.write_uint24_le, -1) ] ;
          equal int ~msg:"nothing written" 0 (Writer.buffered w) ;
          Writer.write_uint8 w 0xff ;
          Writer.write_uint16_le w 0xffff ;
          Writer.write_uint16_be w 0xffff ;
          Writer.write_uint24_le w 0xff_ffff ;
          Writer.write_uint8 w 0 ;
          Writer.flush w ;
          equal string (String.make 8 '\xff' ^ "\x00") (output f) )
    ; test "a full buffer goes to the flow in one write" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          for i = 1 to 9 do
            Writer.write_uint8 w i
          done ;
          equal (list int) [8] (Fake.write_lens f) ;
          Writer.flush w ;
          equal (list int) [8; 1] (Fake.write_lens f) ;
          equal string (String.init 9 (fun i -> Char.chr (i + 1))) (output f) )
    ; test "a large write goes past the buffer" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          let src = Bytes.of_string (Fake.digits 100) in
          Writer.write_uint24_le w 0x434241 ;
          Writer.write_bytes w src 0 100 ;
          equal (list int) ~msg:"the full buffer, then the rest" [8; 95]
            (Fake.write_lens f) ;
          ( match !(f.writes) with
          | {buf; off; _} :: _ ->
              is_true ~msg:"the bytes of the caller" (buf == src) ;
              equal int 5 off
          | [] ->
              fail "no write" ) ;
          equal string ("ABC" ^ Fake.digits 100) (output f) ;
          equal int 0 (Writer.buffered w) ;
          let s = Fake.digits 9 in
          Writer.write_string w s ;
          ( match !(f.writes) with
          | {buf; off; len} :: _ ->
              is_true ~msg:"no copy from an empty buffer"
                (Bytes.unsafe_to_string buf == s) ;
              equal (pair int int) (0, 9) (off, len)
          | [] ->
              fail "no write" ) ;
          Writer.write_string w "xyz" ;
          Writer.write_string w (Fake.digits 9) ;
          equal (list int) ~msg:"a small rest stays" [8; 95; 9; 8]
            (Fake.write_lens f) ;
          equal int 4 (Writer.buffered w) ;
          Writer.write_bytes w src 0 0 ;
          raises_match is_invalid_arg (fun () -> Writer.write_bytes w src 95 6) ;
          raises_match is_invalid_arg (fun () ->
              Writer.write_bytes w src (-1) 2 ) ;
          raises_match is_invalid_arg (fun () ->
              Writer.write_bytes w src 0 (-1) ) ;
          Writer.flush w ;
          equal string
            ("ABC" ^ Fake.digits 100 ^ Fake.digits 9 ^ "xyz" ^ Fake.digits 9)
            (output f) ;
          (* The limits of the rule. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          Writer.write_string w (Fake.digits 8) ;
          equal (list int) ~msg:"8 bytes fit in an empty buffer" []
            (Fake.write_lens f) ;
          Writer.flush w ;
          Writer.write_string w "abc" ;
          Writer.write_string w (Fake.digits 8) ;
          equal (list int) ~msg:"size bytes that do not fit fill the buffer"
            [8; 8] (Fake.write_lens f) ;
          equal int 3 (Writer.buffered w) ;
          let s13 = Fake.digits 13 in
          Writer.write_string w s13 ;
          ( match !(f.writes) with
          | {buf; off; len} :: _ ->
              is_true ~msg:"a rest of size bytes goes with no copy"
                (Bytes.unsafe_to_string buf == s13) ;
              equal (pair int int) (5, 8) (off, len)
          | [] ->
              fail "no write" ) ;
          Writer.write_string w "abcdef" ;
          Writer.write_string w "12345" ;
          equal (list int) ~msg:"a small write that does not fit"
            [8; 8; 8; 8; 6] (Fake.write_lens f) ;
          equal int 5 (Writer.buffered w) ;
          Writer.flush w ;
          equal string
            (Fake.digits 8 ^ "abc" ^ Fake.digits 8 ^ s13 ^ "abcdef" ^ "12345")
            (output f) )
    ; test "a frame: the header gets the length of the body" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create f.flow in
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "abc" ;
          equal int 3 (Writer.end_frame w) ;
          equal int ~msg:"while the header fills" 7 (Writer.buffered w) ;
          Writer.write_uint24_le w 3 ;
          Writer.write_uint8 w 7 ;
          equal int 7 (Writer.buffered w) ;
          Writer.write_uint8 w 9 ;
          Writer.flush w ;
          equal string "\003\000\000\007abc\009" (output f) )
    ; test "an open frame does not go to the flow" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          let body = Fake.digits 100 in
          write w 1 body ;
          equal (list int) [] (Fake.write_lens f) ;
          Writer.flush w ;
          equal (list int) [104] (Fake.write_lens f) ;
          equal string (encode 1 body) (output f) ;
          (* A body in two parts: the growth keeps the bytes. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          Writer.start_frame w ~header:4 ;
          Writer.write_string w (Fake.digits 50) ;
          Writer.write_string w (Fake.digits 50) ;
          let n = Writer.end_frame w in
          Writer.write_uint24_le w n ;
          Writer.write_uint8 w 2 ;
          Writer.flush w ;
          equal string (encode 2 (Fake.digits 50 ^ Fake.digits 50)) (output f) ;
          (* The buffer grows to the frame, and not more. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          write w 1 body ;
          Writer.write_uint8 w 9 ;
          equal (list int) ~msg:"the full buffer" [104] (Fake.write_lens f) ;
          (* A frame at offset 0 does not touch a closed flow. *)
          let flow = Flow.of_string "" (Buffer.create 0) in
          let w = Writer.create ~size:8 flow in
          Writer.start_frame w ~header:4 ;
          Flow.close flow ;
          Writer.write_string w (Fake.digits 10) ;
          equal int 14 (Writer.buffered w) )
    ; test "the bytes before a frame go when the frame needs space" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          Writer.write_string w "abcdef" ;
          Writer.start_frame w ~header:2 ;
          equal (list int) ~msg:"the header fits" [] (Fake.write_lens f) ;
          Writer.write_string w "0123456789" ;
          equal (list int) [6] (Fake.write_lens f) ;
          equal int 10 (Writer.end_frame w) ;
          Writer.write_uint16_be w 10 ;
          Writer.flush w ;
          equal string "abcdef\000\n0123456789" (output f) ;
          (* The header does not fit: the bytes before the frame go first. *)
          Writer.write_string w "abcdefg" ;
          Writer.start_frame w ~header:2 ;
          equal (list int) [6; 12; 7] (Fake.write_lens f) ;
          Writer.write_uint8 w 1 ;
          ignore (Writer.end_frame w) ;
          Writer.write_uint16_be w 1 ;
          Writer.flush w ;
          equal string "abcdef\000\n0123456789abcdefg\000\001\001" (output f) ;
          (* The move carries the bytes of the body, and the frame then fits. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:16 f.flow in
          Writer.write_string w "abcdef" ;
          Writer.start_frame w ~header:2 ;
          Writer.write_string w "0123" ;
          Writer.write_string w "456789abcd" ;
          equal int 14 (Writer.end_frame w) ;
          Writer.write_uint16_be w 14 ;
          Writer.write_uint8 w 0x21 ;
          equal (list int) ~msg:"no growth: the buffer was full" [6; 16]
            (Fake.write_lens f) ;
          Writer.flush w ;
          equal string "abcdef\000\0140123456789abcd!" (output f) )
    ; test "a frame over the maximum raises Too_large and drops the frame"
        (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:16 ~max_frame:8 f.flow in
          Writer.write_string w "ab" ;
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "wxyz" ;
          raises Writer.Too_large (fun () -> Writer.write_uint8 w 1) ;
          equal int ~msg:"the bytes before the frame" 2 (Writer.buffered w) ;
          Writer.write_string w "c" ;
          Writer.flush w ;
          equal string "abc" (output f) ;
          raises Writer.Too_large (fun () -> Writer.start_frame w ~header:9) ;
          Writer.start_frame w ~header:8 ;
          raises Writer.Too_large (fun () -> Writer.write_string w "1") ;
          (* A large frame in a large buffer. *)
          let w = Writer.create ~size:8 ~max_frame:20 f.flow in
          Writer.write_string w "abcdef" ;
          Writer.start_frame w ~header:1 ;
          raises Writer.Too_large (fun () ->
              Writer.write_string w (Fake.digits 20) ) ;
          equal (list int) ~msg:"nothing went" [3] (Fake.write_lens f) ;
          equal int 6 (Writer.buffered w) )
    ; test "the misuses of frames raise Invalid_argument with no change"
        (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          raises_match ~msg:"end with no frame" is_invalid_arg (fun () ->
              Writer.end_frame w ) ;
          raises_match ~msg:"a header of 0" is_invalid_arg (fun () ->
              Writer.start_frame w ~header:0 ) ;
          Writer.start_frame w ~header:2 ;
          raises_match ~msg:"a frame in a frame" is_invalid_arg (fun () ->
              Writer.start_frame w ~header:2 ) ;
          raises_match ~msg:"flush in a body" is_invalid_arg (fun () ->
              Writer.flush w ) ;
          Writer.write_string w "abc" ;
          equal int 3 (Writer.end_frame w) ;
          raises_match ~msg:"end two times" is_invalid_arg (fun () ->
              Writer.end_frame w ) ;
          raises_match ~msg:"flush in the header" is_invalid_arg (fun () ->
              Writer.flush w ) ;
          raises_match ~msg:"a frame in the header" is_invalid_arg (fun () ->
              Writer.start_frame w ~header:2 ) ;
          Writer.write_uint8 w 0 ;
          raises_match ~msg:"a write across the end of the header"
            is_invalid_arg (fun () -> Writer.write_uint16_le w 0 ) ;
          raises_match ~msg:"a large write in the header" is_invalid_arg
            (fun () -> Writer.write_string w (Fake.digits 9) ) ;
          equal int 5 (Writer.buffered w) ;
          Writer.write_uint8 w 3 ;
          Writer.write_uint8 w 0x21 ;
          Writer.flush w ;
          equal string "\000\003abc!" (output f) ;
          equal (list int) [6] (Fake.write_lens f) )
    ; test "a failed write of the flow breaks the writer" (fun () ->
          let check w f =
            equal int 0 (Writer.buffered w) ;
            List.iter
              (fun (name, call) -> raises_match ~msg:name is_invalid_arg call)
              [ ("write_uint8", fun () -> Writer.write_uint8 w 1)
              ; ("write_string", fun () -> Writer.write_string w "")
              ; ( "a large write"
                , fun () -> Writer.write_string w (Fake.digits 9) )
              ; ("start_frame", fun () -> Writer.start_frame w ~header:1)
              ; ("end_frame", fun () -> ignore (Writer.end_frame w))
              ; ("flush", fun () -> Writer.flush w) ] ;
            Writer.abort_frame w ;
            equal int ~msg:"no other write" 1 (List.length !(f.Fake.writes))
          in
          let f = Fake.make ~write_raises:Flow.Timeout "" in
          let w = Writer.create ~size:8 f.flow in
          Writer.write_uint8 w 1 ;
          raises Flow.Timeout (fun () -> Writer.flush w) ;
          check w f ;
          let f = Fake.make ~write_raises:Flow.Closed "" in
          let w = Writer.create ~size:8 f.flow in
          for i = 1 to 8 do
            Writer.write_uint8 w i
          done ;
          raises Flow.Closed (fun () -> Writer.write_uint8 w 9) ;
          check w f ;
          let f = Fake.make ~write_raises:Exit "" in
          let w = Writer.create ~size:8 f.flow in
          Writer.write_string w "abc" ;
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "x" ;
          raises Exit (fun () -> Writer.write_string w "yz") ;
          check w f )
    ; test "flush with no byte does not write the flow" (fun () ->
          let flow = Flow.of_string "" (Buffer.create 0) in
          let w = Writer.create flow in
          Flow.close flow ;
          Writer.flush w ;
          Writer.write_uint8 w 1 ;
          equal int ~msg:"not broken" 1 (Writer.buffered w) )
    ; test "the writer goes back to its size after a large frame" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          write w 0 (Fake.digits 100) ;
          Writer.flush w ;
          for i = 1 to 9 do
            Writer.write_uint8 w i
          done ;
          equal (list int) [104; 8] (Fake.write_lens f) ;
          (* A drop that leaves bytes in a larger buffer keeps them. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:64 f.flow in
          write w 0 (String.make 150 'x') ;
          write w 1 (String.make 10 'y') ;
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "zz" ;
          Writer.abort_frame w ;
          equal int ~msg:"the second frame stays" 14 (Writer.buffered w) ;
          Writer.flush w ;
          equal (list int) [154; 14] (Fake.write_lens f) ;
          equal string
            (encode 0 (String.make 150 'x') ^ encode 1 (String.make 10 'y'))
            (output f) ;
          (* Also after a large frame that the writer dropped. *)
          let nine_bytes drop =
            let f = Fake.make "" in
            let w = Writer.create ~size:8 ~max_frame:1000 f.flow in
            Writer.start_frame w ~header:4 ;
            Writer.write_string w (Fake.digits 900) ;
            drop w ;
            Writer.flush w ;
            for i = 1 to 9 do
              Writer.write_uint8 w i
            done ;
            Fake.write_lens f
          in
          equal (list int) ~msg:"after Too_large" [8]
            (nine_bytes (fun w ->
                 raises Writer.Too_large (fun () ->
                     Writer.write_string w (Fake.digits 200) ) ) ) ;
          equal (list int) ~msg:"after abort_frame" [8]
            (nine_bytes Writer.abort_frame) )
    ; test "a larger buffer of at most the limit stays for the next frame"
        (fun () ->
          (* [frame f w n] writes a frame of [n] bytes and flushes it, and gives
             the buffer that the flow got. *)
          let frame f w n =
            Writer.start_frame w ~header:4 ;
            Writer.write_string w (String.make (n - 4) 'x') ;
            ignore (Writer.end_frame w) ;
            Writer.write_uint24_le w (n - 4) ;
            Writer.write_uint8 w 0 ;
            Writer.flush w ;
            match !(f.Fake.writes) with
            | {buf; _} :: _ ->
                buf
            | [] ->
                fail "no write"
          in
          let f = Fake.make "" in
          let w = Writer.create ~size:8 f.flow in
          let a = frame f w 100 in
          (* A frame at 0 in the first buffer, dropped: the spare stays. *)
          Writer.start_frame w ~header:4 ;
          Writer.abort_frame w ;
          is_true ~msg:"the same larger buffer" (frame f w 100 == a) ;
          let a = frame f w 65536 in
          is_true ~msg:"64 KiB stays" (frame f w 65536 == a) ;
          let b = frame f w 65537 in
          is_true ~msg:"more than 64 KiB does not stay" (frame f w 65537 != b) ;
          let f = Fake.make "" in
          let w = Writer.create ~size:8192 f.flow in
          let c = frame f w 131072 in
          is_true ~msg:"16 * size stays" (frame f w 131072 == c) )
    ; test "create checks its arguments, and the defaults" (fun () ->
          let flow = (Fake.make "").flow in
          raises_match is_invalid_arg (fun () -> Writer.create ~size:7 flow) ;
          raises_match is_invalid_arg (fun () ->
              Writer.create ~max_frame:0 flow ) ;
          ignore (Writer.create ~size:8 ~max_frame:1 flow) ;
          (* A maximum of max_int does not overflow. *)
          let f = Fake.make "" in
          let w = Writer.create ~size:64 ~max_frame:max_int f.flow in
          Writer.write_string w "abc" ;
          Writer.start_frame w ~header:4 ;
          Writer.write_uint8 w 1 ;
          equal (list int) ~msg:"the frame fits" [] (Fake.write_lens f) ;
          equal int 8 (Writer.buffered w) ;
          let f = Fake.make "" in
          let w = Writer.create f.flow in
          for _ = 0 to 4096 do
            Writer.write_uint8 w 0
          done ;
          equal (list int) [4096] (Fake.write_lens f) ;
          Writer.flush w ;
          raises Writer.Too_large (fun () ->
              Writer.start_frame w ~header:((1 lsl 24) + 1) ) ;
          Writer.start_frame w ~header:(1 lsl 24) ;
          equal int (1 lsl 24) (Writer.buffered w) ;
          Writer.abort_frame w )
    ; test "abort_frame drops the frame after an error in the body" (fun () ->
          let f = Fake.make "" in
          let w = Writer.create ~size:64 f.flow in
          Writer.write_string w "ab" ;
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "xyz" ;
          ( match Writer.write_uint8 w 300 with
          | () ->
              fail "no error"
          | exception Invalid_argument _ ->
              Writer.abort_frame w ) ;
          equal int 2 (Writer.buffered w) ;
          write w 1 "q" ;
          (* An abort while the header fills. *)
          Writer.start_frame w ~header:4 ;
          Writer.write_string w "lost" ;
          ignore (Writer.end_frame w) ;
          Writer.write_uint8 w 1 ;
          Writer.abort_frame w ;
          equal int 7 (Writer.buffered w) ;
          Writer.abort_frame w ;
          Writer.flush w ;
          equal string ("ab" ^ encode 1 "q") (output f) ) ]

(* The stateful test. The model is all the bytes that the writes made, in order.
   The flow gets a prefix of them, and all of them after a flush. *)

type model = {expected: Buffer.t}

type sys = {w: Writer.t; f: Fake.t}

let max_frame = 64

let invariant m s =
  let out = output s.f and all = Buffer.contents m.expected in
  is_true ~msg:"the output is a prefix of the writes"
    ( String.length out <= String.length all
    && String.sub all 0 (String.length out) = out ) ;
  equal int ~msg:"buffered"
    (String.length all - String.length out)
    (Writer.buffered s.w)

let writer =
  abstract "w" ~invariant ~pp:(fun ppf m ->
      Format.fprintf ppf "%d bytes" (Buffer.length m.expected) )

let m_op op m = reference m.expected op

let s_op op s = apply s.w op

let m_frame (seq, payload) m =
  if 4 + String.length payload > max_frame then "Too_large"
  else begin
    Buffer.add_string m.expected (encode seq payload) ;
    "ok"
  end

let s_frame (seq, payload) s =
  match write s.w seq payload with
  | () ->
      "ok"
  | exception Writer.Too_large ->
      "Too_large"

let m_flush () m = Buffer.contents m.expected

let s_flush () s = Writer.flush s.w ; output s.f

let commands =
  [ command "create"
      (Gen.unit @-> makes writer)
      (fun () -> {expected= Buffer.create 64})
      (fun () ->
        let f = Fake.make "" in
        {w= Writer.create ~size:8 ~max_frame f.flow; f} )
  ; command "write" (gen_op @-> writer ^-> returns unit) m_op s_op
  ; command "packet"
      ( Gen.(pair (int_range 0 255) (string_of ~size:(int_range 0 70) char))
      @-> writer ^-> returns string )
      m_frame s_frame
  ; command "flush" (Gen.unit @-> writer ^-> returns string) m_flush s_flush ]

let stateful_test =
  group "Writer against a model"
    [stateful "the writer behaves as the model" ~count:300 ~steps:30 commands]

let tests = [unit_tests; stateful_test]
