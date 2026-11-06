(* The tests of Reader and Writer with the models of framing.ml: the codecs of
   codec.ml give the bytes and the values of the models, for each split of the
   input and each size of buffer. The group "Reader and Writer on sockets" runs
   the codecs on the TCP flows of the simulator. *)

open Windtrap
open Weft_flow
open Framing
open Codec

(* The generators *)

let gen_packets =
  Gen.(
    list ~size:(int_range 0 12)
      (map
         (fun (seq, payload) -> {Mysql_model.seq; payload})
         (pair (int_range 0 255) (string_of ~size:(int_range 0 600) char)) )
    |> with_pp (Format.pp_print_list Mysql_model.pp_packet) )

let pp_packets = Format.pp_print_list Mysql_model.pp_packet

(* The limits of the prefixes of a length-encoded integer, and the values next
   to them. *)
let lenenc_limits =
  [ 0L
  ; 250L
  ; 251L
  ; 0xffffL
  ; 0x1_0000L
  ; 0xff_ffffL
  ; 0x100_0000L
  ; Int64.max_int
  ; -1L ]

let gen_field =
  Gen.(
    one_of
      [ map
          (fun v -> Mysql_model.Int v)
          (of_list ~pp:(fun ppf v -> Format.fprintf ppf "%Lu" v) lenenc_limits)
      ; map (fun v -> Mysql_model.Int v) int64
      ; map (fun v -> Mysql_model.Int (Int64.of_int v)) (int_range 0 0x1ff_ffff)
      ; map
          (fun s -> Mysql_model.Text s)
          (string_of ~size:(int_range 0 30) (char_range '\001' '\255')) ] )

let gen_rows =
  Gen.(
    list ~size:(int_range 0 6) (list ~size:(int_range 0 8) gen_field)
    |> with_pp
         (Format.pp_print_list (Format.pp_print_list Mysql_model.pp_field)) )

(* Payload lengths at the limits of the 7-bit, 16-bit and 64-bit forms, and
   small random lengths. *)
let gen_ws_frames =
  Gen.(
    list ~size:(int_range 0 5)
      (map
         (fun ((fin, opcode), (key, len)) ->
           { Ws_model.fin
           ; opcode
           ; key
           ; payload= String.init len (fun i -> Char.chr (i * 7 land 0xff)) } )
         (pair
            (pair bool (of_list ~pp:Format.pp_print_int [0; 1; 2; 8; 9; 10]))
            (pair
               (option (string_of ~size:(int_range 4 4) char))
               (one_of
                  [ of_list ~pp:Format.pp_print_int
                      [0; 1; 125; 126; 127; 65535; 65536; 70000]
                  ; int_range 0 300 ] ) ) ) )
    |> with_pp (Format.pp_print_list Ws_model.pp_frame) )

(* A split of the input and a size of the buffer of the reader. *)
let gen_split = Gen.(pair (int_range 1 9) (int_range 1 16))

(* [written ~size f] gives the bytes that [f] writes to a writer of [size]
   bytes, after one flush. *)
let written ~size f =
  let out = Buffer.create 64 in
  let w = Writer.create ~size (Flow.of_string "" out) in
  f w ; Writer.flush w ; Buffer.contents out

let packets = Windtrap.Testable.make ~pp:pp_packets ~equal:( = )

let rows =
  Windtrap.Testable.make
    ~pp:(Format.pp_print_list (Format.pp_print_list Mysql_model.pp_field))
    ~equal:( = )

let frames =
  Windtrap.Testable.make
    ~pp:(Format.pp_print_list Ws_model.pp_frame)
    ~equal:( = )

let reader ~chunk ~size input = Test_reader.of_string ~chunk ~size input

let model_tests =
  group "Framing models"
    [ prop "MySQL packets: a stream cut at each place gives the complete packets"
        Gen.(triple gen_packets gen_split (option nat))
        (fun (ps, (chunk, size), cut) ->
          let all = Mysql_model.encode ps in
          (* No cut: all the stream. *)
          let cut =
            match cut with
            | None ->
                String.length all
            | Some c ->
                c mod (String.length all + 1)
          in
          let input = String.sub all 0 cut in
          let complete, used = Mysql_model.decode input in
          let r = reader ~chunk ~size input in
          equal packets complete (Mysql.read_packets r) ;
          let rest = cut - used in
          equal int ~msg:"the bytes of the cut packet"
            (if rest < 4 then rest else rest - 4)
            (Reader.buffered r) )
    ; prop "MySQL rows: length-encoded integers and NUL strings"
        Gen.(triple gen_rows gen_split (int_range 8 64))
        (fun (rs, (chunk, rsize), wsize) ->
          let bytes =
            written ~size:wsize (fun w ->
                List.iteri (fun i row -> Mysql.write_row w i row) rs )
          in
          equal string ~msg:"the bytes of the model"
            (Mysql_model.encode
               (List.mapi
                  (fun i row ->
                    {Mysql_model.seq= i; payload= Mysql_model.row row} )
                  rs ) )
            bytes ;
          let r = reader ~chunk ~size:rsize bytes in
          let kinds row =
            List.map
              (function Mysql_model.Text _ -> true | Int _ -> false)
              row
          in
          equal rows rs (List.map (fun row -> Mysql.read_row r (kinds row)) rs) )
    ; test "MySQL rows: a field that ends after its packet is an error"
        (fun () ->
          let r =
            reader ~chunk:64 ~size:64
              (Mysql_model.encode
                 [ {seq= 0; payload= "\xfc\x01"}
                 ; {seq= 1; payload= "\005abc\000"} ] )
          in
          raises (Codec.Error "truncated") (fun () -> Mysql.read_row r [false]) ;
          is_true ~msg:"the reader holds the next packet" (Reader.buffered r > 2) ;
          let r =
            reader ~chunk:64 ~size:64
              (Mysql_model.encode
                 [{seq= 0; payload= "abc"}; {seq= 1; payload= "\000"}] )
          in
          raises (Codec.Error "no NUL") (fun () -> Mysql.read_row r [true]) )
    ; prop "WebSocket frames: the writer gives the bytes of the model"
        Gen.(pair gen_ws_frames (int_range 8 64))
        (fun (fs, size) ->
          equal string (Ws_model.encode fs)
            (written ~size (fun w -> List.iter (Ws.write_frame w) fs)) )
    ; prop "WebSocket frames: the reader decodes the bytes of the model"
        Gen.(pair gen_ws_frames gen_split)
        (fun (fs, (chunk, size)) ->
          let r = reader ~chunk ~size (Ws_model.encode fs) in
          equal frames fs (Ws.read_frames r) ) ]

(* The simulator tests: the codecs on the TCP flows of the simulator, with short
   reads and writes, deadlines, cancels and faults. *)

open Weft
open Helpers

let payload i n = String.init n (fun k -> Char.chr ((i + k) land 0xff))

(* [echo sim ~size ~flush_every ps] sends the packets [ps] from the client, with
   a flush after each [flush_every] packets. The server sends each packet back.
   The client gets the same packets. The readers and the writers have [size]
   bytes. *)
let echo sim ~size ~flush_every ps =
  with_flows sim
  @@ fun ~server ~client ->
  Fiber.scope
  @@ fun s ->
  let back =
    call s (fun () ->
        let r = Reader.create ~size server and w = Writer.create ~size server in
        List.iter
          (fun _ ->
            Mysql.write_packet w (Mysql.read_packet r) ;
            Writer.flush w )
          ps )
  in
  let send =
    call s (fun () ->
        let w = Writer.create ~size client in
        List.iteri
          (fun i p ->
            Mysql.write_packet w p ;
            if (i + 1) mod flush_every = 0 then Writer.flush w )
          ps ;
        Writer.flush w )
  in
  let r = Reader.create ~size client in
  equal packets ps (List.map (fun _ -> Mysql.read_packet r) ps) ;
  await send ;
  await back

let sim_tests =
  group "Reader and Writer on sockets"
    [ test "MySQL packets each way with short reads and writes" (fun () ->
          run
          @@ fun sim ->
          Weft_sim.net_faults sim {Weft_sim.no_faults with short_io= true} ;
          echo sim ~size:4096 ~flush_every:10
            (List.init 100 (fun i ->
                 { Mysql_model.seq= i land 0xff
                 ; payload= payload i (i * 7919 mod 70_001) } ) ) )
    ; test "race: a reader and a writer on one flow in two fibers" (fun () ->
          race
          @@ fun sim ->
          echo sim ~size:16 ~flush_every:1
            (List.init 20 (fun i ->
                 {Mysql_model.seq= i; payload= payload i (i * 37)} ) ) )
    ; test "an exception in ensure keeps the bytes that came" (fun () ->
          (* Each case: the action on the flows while ensure waits, the
             exception of ensure, and the check after it. *)
          let continues r ~server ~client =
            Flow.set_read_deadline server None ;
            Flow.write_string client "\003\004" ;
            Reader.ensure r 4 ;
            equal int32 0x04030201l (Reader.get_int32_le r 0)
          in
          let cases =
            [ ( "a cancel"
              , (fun c ~server:_ ~client:_ -> cancel c)
              , Promise.Cancelled
              , continues )
            ; ( "a passed read deadline"
              , (fun _ ~server ~client:_ ->
                  Flow.set_read_deadline server (Some (Time.now Mono)) )
              , Flow.Timeout
              , continues )
            ; ( "a close"
              , (fun _ ~server ~client:_ -> Flow.close server)
              , Flow.Closed
              , fun r ~server:_ ~client:_ ->
                  raises Flow.Closed (fun () -> Reader.ensure r 4) )
            ; ( "a reset of the peer"
              , (fun _ ~server:_ ~client -> Flow.reset client)
              , Flow.Closed
              , fun _ ~server:_ ~client:_ -> () )
            ; ( "the end of the stream"
              , (fun _ ~server:_ ~client -> Flow.shutdown_send client)
              , End_of_file
              , fun r ~server:_ ~client:_ ->
                  raises End_of_file (fun () -> Reader.ensure r 3) ) ]
          in
          List.iter
            (fun (name, act, exn, after) ->
              run
              @@ fun sim ->
              with_flows sim
              @@ fun ~server ~client ->
              Fiber.scope
              @@ fun s ->
              Flow.write_string client "\001\002" ;
              let r = Reader.create server in
              let c = call s (fun () -> Reader.ensure r 4) in
              Weft_sim.settle sim ;
              is_true ~msg:(name ^ ": ensure waits") (not (ended sim c)) ;
              act c ~server ~client ;
              raises ~msg:name exn (fun () -> await c) ;
              equal int ~msg:(name ^ ": the bytes stay") 2 (Reader.buffered r) ;
              equal int ~msg:(name ^ ": nothing consumed") 0 (Reader.consumed r) ;
              after r ~server ~client )
            cases )
    ; test "a cancel of a flush that waits breaks the writer, and leaves no fd"
        (fun () ->
          run
          @@ fun sim ->
          no_fd_leak sim
          @@ fun () ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Fiber.scope
          @@ fun s ->
          let w = Writer.create ~size:(big + 1) server in
          Writer.write_string w (String.make big 'x') ;
          let c = call s (fun () -> Writer.flush w) in
          Weft_sim.settle sim ;
          is_true ~msg:"the flush waits" (not (ended sim c)) ;
          cancel c ;
          raises Promise.Cancelled (fun () -> await c) ;
          raises_match is_invalid_arg (fun () -> Writer.write_uint8 w 1) ;
          Flow.close server ) ]

let tests = [model_tests; sim_tests]
