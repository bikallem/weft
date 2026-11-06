(* The tests of Reader (design section 2.18): the unit tests on the fake flow of
   fake.ml and on Flow.of_string, a property, and a stateful test against a
   model. They need no fiber. *)

open Windtrap
open Weft_flow

let is_invalid_arg = Helpers.is_invalid_arg

let of_string ?size ?chunk input =
  Reader.create ?size (Flow.of_string ?chunk input (Buffer.create 0))

let reads f = List.length !(f.Fake.reads)

(* The getters of the reader, with the width of their integer, and the same
   getter on a string. The values are strings, so one list holds all types. *)
let getters =
  let i = string_of_int and i32 = Int32.to_string and i64 = Int64.to_string in
  let s = Bytes.unsafe_of_string in
  [ ( "uint8"
    , 1
    , (fun r o -> i (Reader.get_uint8 r o))
    , fun b o -> i (Bytes.get_uint8 (s b) o) )
  ; ( "uint16_le"
    , 2
    , (fun r o -> i (Reader.get_uint16_le r o))
    , fun b o -> i (Bytes.get_uint16_le (s b) o) )
  ; ( "uint16_be"
    , 2
    , (fun r o -> i (Reader.get_uint16_be r o))
    , fun b o -> i (Bytes.get_uint16_be (s b) o) )
  ; ( "uint24_le"
    , 3
    , (fun r o -> i (Reader.get_uint24_le r o))
    , fun b o ->
        i
          ( Bytes.get_uint8 (s b) o
          lor (Bytes.get_uint8 (s b) (o + 1) lsl 8)
          lor (Bytes.get_uint8 (s b) (o + 2) lsl 16) ) )
  ; ( "int32_le"
    , 4
    , (fun r o -> i32 (Reader.get_int32_le r o))
    , fun b o -> i32 (Bytes.get_int32_le (s b) o) )
  ; ( "int64_le"
    , 8
    , (fun r o -> i64 (Reader.get_int64_le r o))
    , fun b o -> i64 (Bytes.get_int64_le (s b) o) )
  ; ( "int64_be"
    , 8
    , (fun r o -> i64 (Reader.get_int64_be r o))
    , fun b o -> i64 (Bytes.get_int64_be (s b) o) ) ]

let unit_tests =
  group "Reader"
    [ test "ensure with sufficient bytes does not read the flow" (fun () ->
          let f = Fake.make "abcdefgh" in
          let r = Reader.create f.flow in
          Reader.ensure r 8 ;
          equal int 1 (reads f) ;
          Reader.ensure r 3 ;
          Reader.ensure r 8 ;
          equal int ~msg:"no other read" 1 (reads f) )
    ; test "a refill asks for the free space of the buffer" (fun () ->
          let f = Fake.make "abc" in
          let r = Reader.create ~size:16 f.flow in
          Reader.ensure r 1 ;
          equal (list int) [16] (Fake.read_lens f) ;
          equal int ~msg:"all the bytes that came" 3 (Reader.buffered r) )
    ; test "an end in a value raises End_of_file and keeps the bytes" (fun () ->
          let f = Fake.make "abc" in
          let r = Reader.create f.flow in
          raises End_of_file (fun () -> Reader.ensure r 4) ;
          equal int 3 (Reader.buffered r) ;
          equal string "abc" (Reader.sub_string r 0 3) ;
          equal int 2 (reads f) ;
          raises End_of_file (fun () -> Reader.ensure r 4) ;
          equal int ~msg:"no read after the end" 2 (reads f) ;
          Reader.ensure r 3 ;
          equal string ~msg:"the bytes stay" "abc" (Reader.sub_string r 0 3) )
    ; test "an end between two values leaves no byte" (fun () ->
          let r = of_string "ab" in
          Reader.ensure r 2 ;
          Reader.consume r 2 ;
          raises End_of_file (fun () -> Reader.ensure r 1) ;
          equal int 0 (Reader.buffered r) )
    ; test "ensure 0 reads nothing, and a negative count is refused" (fun () ->
          let f = Fake.make "" in
          let r = Reader.create f.flow in
          Reader.ensure r 0 ;
          equal int 0 (reads f) ;
          raises End_of_file (fun () -> Reader.ensure r 1) ;
          Reader.ensure r 0 ;
          raises_match is_invalid_arg (fun () -> Reader.ensure r (-1)) )
    ; test "ensure over the maximum raises Too_large and reads nothing"
        (fun () ->
          let f = Fake.make "" in
          let r = Reader.create f.flow in
          raises Reader.Too_large (fun () -> Reader.ensure r ((1 lsl 24) + 1)) ;
          equal int 0 (reads f) ;
          raises End_of_file (fun () -> Reader.ensure r (1 lsl 24)) ;
          raises ~msg:"Too_large wins over the end" Reader.Too_large (fun () ->
              Reader.ensure r ((1 lsl 24) + 1) ) ;
          let big = Reader.create ~size:((1 lsl 24) + 8) (Fake.make "").flow in
          raises ~msg:"the maximum is the size when it is larger" End_of_file
            (fun () -> Reader.ensure big ((1 lsl 24) + 8) ) )
    ; test "the default size, and an invalid size" (fun () ->
          let f = Fake.make "x" in
          let r = Reader.create f.flow in
          Reader.ensure r 1 ;
          equal (list int) [4096] (Fake.read_lens f) ;
          raises_match is_invalid_arg (fun () ->
              Reader.create ~size:0 (Fake.make "").flow ) ;
          ignore (Reader.create ~size:1 (Fake.make "").flow) )
    ; test "the buffer grows with the bytes that came" (fun () ->
          let input = Fake.digits 30 in
          let f = Fake.make ~chunk:3 input in
          let r = Reader.create ~size:4 f.flow in
          let b0 = Reader.buffer r in
          Reader.ensure r 20 ;
          equal (list int) [4; 1; 4; 1; 8; 5; 2; 4; 1] (Fake.read_lens f) ;
          equal string (String.sub input 0 20) (Reader.sub_string r 0 20) ;
          is_true ~msg:"a new buffer" (Reader.buffer r != b0) ;
          let f = Fake.make (Fake.digits 10) in
          let r = Reader.create ~size:4 f.flow in
          raises End_of_file (fun () -> Reader.ensure r (1 lsl 20)) ;
          equal (list int) ~msg:"the largest read is 8" [4; 4; 8; 6]
            (Fake.read_lens f) )
    ; test "a move keeps the bytes in order" (fun () ->
          let r = of_string ~size:8 ~chunk:6 "0123456789abcdef" in
          let b0 = Reader.buffer r in
          Reader.ensure r 6 ;
          Reader.consume r 5 ;
          Reader.ensure r 6 ;
          equal string "56789a" (Reader.sub_string r 0 6) ;
          is_true ~msg:"the same buffer" (Reader.buffer r == b0) ;
          (* A value that fits at its place does not move. *)
          let f = Fake.make ~chunk:4 (Fake.digits 12) in
          let r = Reader.create ~size:8 f.flow in
          Reader.ensure r 4 ;
          Reader.consume r 2 ;
          Reader.ensure r 6 ;
          equal (list int) ~msg:"the free space after the bytes" [8; 4]
            (Fake.read_lens f) ;
          equal string "234567" (Reader.sub_string r 0 6) ;
          (* A value of the size of the buffer moves to its start. *)
          let f = Fake.make ~chunk:4 (Fake.digits 12) in
          let r = Reader.create ~size:8 f.flow in
          let b0 = Reader.buffer r in
          Reader.ensure r 4 ;
          Reader.consume r 2 ;
          Reader.ensure r 8 ;
          equal (list int) [8; 6; 2] (Fake.read_lens f) ;
          is_true ~msg:"no new buffer" (Reader.buffer r == b0) ;
          equal string "23456789" (Reader.sub_string r 0 8) )
    ; test "a growth keeps the bytes after a consume" (fun () ->
          let r = of_string ~size:4 "0123456789abcdef" in
          Reader.ensure r 4 ;
          Reader.consume r 2 ;
          Reader.ensure r 10 ;
          equal string "23456789ab" (Reader.sub_string r 0 10) )
    ; test "the reader goes back to its size after a large value" (fun () ->
          let input = Fake.digits 22 in
          let f = Fake.make input in
          let r = Reader.create ~size:8 f.flow in
          let b0 = Reader.buffer r in
          Reader.ensure r 20 ;
          is_true ~msg:"a large buffer" (Reader.buffer r != b0) ;
          Reader.consume r 20 ;
          is_true ~msg:"no byte: the first buffer" (Reader.buffer r == b0) ;
          Reader.ensure r 1 ;
          equal (list int) [8; 8; 4; 8] (Fake.read_lens f) ;
          equal string "kl" (Reader.sub_string r 0 2) )
    ; test "a larger buffer of at most the limit stays for the next growth"
        (fun () ->
          let f = Fake.make (Fake.digits 100) in
          let r = Reader.create ~size:8 f.flow in
          Reader.ensure r 20 ;
          let large = Reader.buffer r in
          Reader.consume r 20 ;
          (* The first buffer, emptied, does not take the place of the spare. *)
          Reader.ensure r 1 ;
          Reader.consume r (Reader.buffered r) ;
          Reader.ensure r 20 ;
          is_true ~msg:"the same larger buffer" (Reader.buffer r == large) ;
          equal (list int) [8; 8; 4; 8; 8; 12] (Fake.read_lens f) ;
          equal string
            (String.sub (Fake.digits 100) 28 20)
            (Reader.sub_string r 0 20) ;
          (* A spare of exactly the size that the growth wants. *)
          let r = of_string ~size:8 (String.make 100 'x') in
          Reader.ensure r 16 ;
          let exact = Reader.buffer r in
          Reader.consume r 16 ;
          Reader.ensure r 16 ;
          is_true ~msg:"a spare of the size of the growth"
            (Reader.buffer r == exact) ;
          (* The limit is 64 KiB for a size of 4 KiB or less. *)
          let r = of_string ~size:8 (String.make 300_000 'x') in
          let grow n =
            Reader.ensure r n ;
            let b = Reader.buffer r in
            Reader.consume r n ; b
          in
          let a = grow 65536 in
          is_true ~msg:"64 KiB stays" (grow 65536 == a) ;
          let b = grow 65537 in
          is_true ~msg:"more than 64 KiB does not stay" (grow 65537 != b) ;
          (* The limit is 16 times a larger size. *)
          let r = of_string ~size:8192 (String.make 300_000 'x') in
          Reader.ensure r 131072 ;
          let c = Reader.buffer r in
          Reader.consume r 131072 ;
          Reader.ensure r 131072 ;
          is_true ~msg:"16 * size stays" (Reader.buffer r == c) )
    ; prop "get_* decode as Bytes.get_* at the offset, and refuse other offsets"
        Gen.(
          quad
            (string_of ~size:(int_range 8 64) char)
            (int_range 1 9) (int_range 1 16)
            (pair (int_range 0 63) (int_range 0 63)) )
        (fun (input, chunk, size, (c, off)) ->
          let n = String.length input in
          let c = c mod n in
          let r = of_string ~size ~chunk input in
          Reader.ensure r n ;
          Reader.consume r c ;
          (* An offset from -1 to the last byte. *)
          let off = (off mod (n - c + 1)) - 1 in
          List.iter
            (fun (name, width, get, model) ->
              if off >= 0 && off + width <= n - c then
                equal string ~msg:name (model input (c + off)) (get r off)
              else raises_match ~msg:name is_invalid_arg (fun () -> get r off) )
            getters )
    ; test "view and buffer give the bytes in place" (fun () ->
          let r = of_string ~chunk:5 (Fake.digits 12) in
          Reader.ensure r 12 ;
          Reader.consume r 3 ;
          let v = Reader.view r 2 4 in
          equal string "5678" (Bytes.sub_string (Reader.buffer r) v 4) ;
          ignore (Reader.view r 9 0) ;
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () -> Reader.view r off len) ;
              raises_match is_invalid_arg (fun () ->
                  Reader.sub_string r off len ) )
            [(-1, 1); (0, -1); (9, 1); (5, 5)] ;
          equal string "3456" (Reader.sub_string r 0 4) )
    ; test "consume refuses more than the buffered bytes" (fun () ->
          let r = of_string "abcdef" in
          Reader.ensure r 6 ;
          raises_match is_invalid_arg (fun () -> Reader.consume r (-1)) ;
          raises_match is_invalid_arg (fun () -> Reader.consume r 7) ;
          equal int 6 (Reader.buffered r) ;
          equal int 0 (Reader.consumed r) ;
          Reader.consume r 4 ;
          equal int 4 (Reader.consumed r) ;
          equal string "ef" (Reader.sub_string r 0 2) )
    ; test "read_into gives the held bytes first, then reads the flow"
        (fun () ->
          let f = Fake.make ~chunk:5 (Fake.digits 40) in
          let r = Reader.create ~size:8 f.flow in
          Reader.ensure r 3 ;
          let dst = Bytes.make 20 '.' in
          equal int ~msg:"the held bytes, no read" 3
            (Reader.read_into r dst 1 3) ;
          equal string ".012" (Bytes.sub_string dst 0 4) ;
          equal int ~msg:"the rest of the held bytes" 2
            (Reader.read_into r dst 0 20) ;
          equal string "34" (Bytes.sub_string dst 0 2) ;
          equal int ~msg:"no read for the held bytes" 1 (reads f) ;
          equal int ~msg:"a large part with no copy" 5
            (Reader.read_into r dst 2 8) ;
          ( match !(f.reads) with
          | {buf; off; len} :: _ ->
              is_true ~msg:"the flow got dst" (buf == dst) ;
              equal (pair int int) (2, 8) (off, len)
          | [] ->
              fail "no read" ) ;
          equal string "56789" (Bytes.sub_string dst 2 5) ;
          equal int ~msg:"a small part fills the buffer" 2
            (Reader.read_into r dst 0 2) ;
          equal string "ab" (Bytes.sub_string dst 0 2) ;
          equal int 3 (Reader.buffered r) ;
          equal (list int) [8; 8; 8] (Fake.read_lens f) ;
          equal int 12 (Reader.consumed r) ;
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Reader.read_into r dst off len ) )
            [(0, 0); (-1, 2); (19, 2)] ;
          let r = of_string "ab" in
          let dst = Bytes.create 8 in
          equal int 2 (Reader.read_into r dst 0 8) ;
          equal int ~msg:"the end" 0 (Reader.read_into r dst 0 8) ;
          equal int ~msg:"the end stays" 0 (Reader.read_into r dst 0 1) ;
          let r = of_string "" in
          equal int ~msg:"a small read at the end" 0
            (Reader.read_into r dst 0 1) ;
          raises End_of_file (fun () -> Reader.ensure r 1) ;
          raises_match ~msg:"an invalid range at the end" is_invalid_arg
            (fun () -> Reader.read_into r dst 7 2 ) ) ]

(* The stateful test. The model is the input and the number of bytes that the
   reader consumed. The reader can hold more bytes than the model knows, because
   a refill reads all the free space. Thus the model keeps [known]: the bytes
   that the last ensure made sure of, less the bytes consumed after it. A
   command reads only in [known] bytes, or at or after the end of the input,
   where the result is sure. *)

type model = {input: string; mutable at: int; mutable known: int}

let remaining m = String.length m.input - m.at

let pp_model ppf m = Format.fprintf ppf "at %d, known %d" m.at m.known

let invariant m r =
  equal int ~msg:"consumed" m.at (Reader.consumed r) ;
  is_true ~msg:"the known bytes are held" (Reader.buffered r >= m.known) ;
  is_true ~msg:"no byte after the input" (Reader.buffered r <= remaining m)

let reader = abstract "r" ~pp:pp_model ~invariant

(* The outcome of a call as a string, also for an exception. *)
let outcome f =
  match f () with
  | v ->
      v
  | exception End_of_file ->
      "End_of_file"
  | exception Invalid_argument _ ->
      "Invalid_argument"

let m_ensure n m =
  if n <= m.known then "ok"
  else if n > remaining m then begin
    m.known <- remaining m ;
    "End_of_file"
  end
  else begin
    m.known <- n ;
    "ok"
  end

let s_ensure n r = outcome (fun () -> Reader.ensure r n ; "ok")

let m_consume k m =
  m.at <- m.at + k ;
  m.known <- m.known - k ;
  "ok"

let s_consume k r = outcome (fun () -> Reader.consume r k ; "ok")

(* A read at [off] of [width] bytes is sure in the known bytes, or after the
   remaining input. *)
let sure m off width = off + width <= m.known || off + width > remaining m

let m_read width f off m =
  if off + width <= m.known then f m.input (m.at + off) else "Invalid_argument"

let m_get16 = m_read 2 (fun s o -> string_of_int (String.get_uint16_le s o))

let s_get16 off r =
  outcome (fun () -> string_of_int (Reader.get_uint16_le r off))

let m_get24 =
  m_read 3 (fun s o ->
      string_of_int
        (String.get_uint16_le s o lor (String.get_uint8 s (o + 2) lsl 16)) )

let s_get24 off r =
  outcome (fun () -> string_of_int (Reader.get_uint24_le r off))

let m_sub (off, len) m = m_read len (fun s o -> String.sub s o len) off m

let s_sub (off, len) r = outcome (fun () -> Reader.sub_string r off len)

(* read_into: a relation. The count is from 1 to [len], and the bytes are the
   next bytes of the input. The count is 0 only at the end of the input. *)
let m_read_into len m got =
  match got with
  | Error e ->
      raise e
  | Ok s ->
      let n = String.length s in
      if remaining m = 0 then equal int ~msg:"0 at the end" 0 n
      else begin
        is_true ~msg:"a count from 1 to len" (n >= 1 && n <= len) ;
        equal string ~msg:"the next bytes" (String.sub m.input m.at n) s
      end ;
      m.at <- m.at + n ;
      m.known <- Int.max 0 (m.known - n)

let s_read_into len r =
  let buf = Bytes.create len in
  let n = Reader.read_into r buf 0 len in
  Bytes.sub_string buf 0 n

let pp_input = Format.pp_print_string

let commands =
  let count = Gen.int_range 0 40 and off = Gen.int_range 0 40 in
  [ command "create"
      ( Gen.with_pp pp_input (Gen.string_of ~size:(Gen.int_range 0 60) Gen.char)
      @-> makes reader )
      (fun input -> {input; at= 0; known= 0})
      (fun input -> of_string ~size:4 ~chunk:3 input)
  ; command "ensure" (count @-> reader ^-> returns string) m_ensure s_ensure
  ; command "consume"
      ~pre:(fun k m -> k <= m.known)
      (count @-> reader ^-> returns string)
      m_consume s_consume
  ; command "get_uint16_le"
      ~pre:(fun o m -> sure m o 2)
      (off @-> reader ^-> returns string)
      m_get16 s_get16
  ; command "get_uint24_le"
      ~pre:(fun o m -> sure m o 3)
      (off @-> reader ^-> returns string)
      m_get24 s_get24
  ; command "sub_string"
      ~pre:(fun (o, l) m -> sure m o l)
      (Gen.pair off (Gen.int_range 0 12) @-> reader ^-> returns string)
      m_sub s_sub
  ; command "read_into"
      (Gen.int_range 1 12 @-> reader ^-> judges string)
      m_read_into s_read_into ]

let stateful_test =
  group "Reader against a model"
    [stateful "the reader behaves as the model" ~count:300 ~steps:30 commands]

let tests = [unit_tests; stateful_test]
