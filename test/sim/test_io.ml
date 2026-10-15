(* The tests of the I/O of the simulator: the files, the edges and the names of
   the network, and the fair reads (design sections 2.6 and 2.7). *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched

let error_of = Portable.error_of

let files =
  group "files"
    [ test "an open with O_CREAT of a path that ends with / gives EISDIR"
        (fun () ->
          Sim.run (fun _ ->
              equal string "EISDIR"
                (error_of (fun () -> File.openfile [Write; Create] "/new/")) ;
              equal string "ENOENT" (error_of (fun () -> File.stat "/new")) ) )
    ; test "a file that grows keeps its data" (fun () ->
          Sim.run (fun _ ->
              let f = File.openfile [Read; Write; Create] "/f" in
              File.write f "abc" ;
              equal int 1
                (File.pwrite f ~file_offset:1000 (Bytes.of_string "x") 0 1) ;
              let buf = Bytes.create 1001 in
              equal int 1001 (File.pread f ~file_offset:0 buf 0 1001) ;
              equal string
                ("abc" ^ String.make 997 '\000' ^ "x")
                (Bytes.to_string buf) ;
              File.close f ) )
    ; test "a truncate past the end gives zeros" (fun () ->
          Sim.run (fun _ ->
              let f = File.openfile [Read; Write; Create] "/f" in
              File.write f "abc" ;
              File.truncate f 1000 ;
              equal int 1000 (File.fstat f).size ;
              let buf = Bytes.create 1000 in
              equal int 1000 (File.pread f ~file_offset:0 buf 0 1000) ;
              equal string
                ("abc" ^ String.make 997 '\000')
                (Bytes.to_string buf) ;
              File.close f ) )
    ; test "a rename of a directory to itself does nothing" (fun () ->
          Sim.run (fun _ ->
              Dir.mkdir "/d" ;
              let f = File.openfile [Write; Create] "/d/x" in
              File.close f ;
              Dir.rename "/d" "/d" ;
              equal (list string) ["x"] (Dir.readdir "/d") ) )
    ; test "a .. goes to the parent, and stays at the root" (fun () ->
          Sim.run (fun _ ->
              Dir.mkdir ~perm:0o700 "/a" ;
              Dir.mkdir "/a/b" ;
              File.close (File.openfile [Write; Create] "/a/b/../f") ;
              File.close (File.openfile [Write; Create] "/../../g") ;
              equal (list string) ["a"; "g"]
                (List.sort compare (Dir.readdir "/")) ;
              equal (list string) ["b"; "f"]
                (List.sort compare (Dir.readdir "/a/b/..")) ;
              equal int 0o700 (File.stat "/a/b/..").perm ;
              let d = File.openfile [Read] "/a/b/.." in
              equal int 0o700 (File.fstat d).perm ;
              File.close d ;
              (* An rmdir of a last ".." gives ENOTEMPTY, as on Linux. *)
              List.iter
                (fun path ->
                  equal ~msg:path string "ENOTEMPTY"
                    (error_of (fun () -> Dir.rmdir path)) )
                ["/a/b/.."; "/a/.."; "/.."] ;
              equal (list string) ["b"; "f"]
                (List.sort compare (Dir.readdir "/a")) ) )
    ; test "an open of a directory reads no data" (fun () ->
          Sim.run (fun _ ->
              Dir.mkdir "/d" ;
              List.iter
                (fun path ->
                  let d = File.openfile [Read] path in
                  is_true ((File.fstat d).kind = File.Directory) ;
                  equal string "EISDIR"
                    (error_of (fun () -> File.read d (Bytes.create 1) 0 1)) ;
                  File.close d ;
                  equal string "EISDIR"
                    (error_of (fun () -> File.openfile [Write] path)) )
                ["/"; "/d"] ) )
    ; test "the root refuses the changes that Linux refuses" (fun () ->
          Sim.run (fun _ ->
              Dir.mkdir "/d" ;
              equal string "EEXIST" (error_of (fun () -> Dir.mkdir "/")) ;
              equal string "EISDIR" (error_of (fun () -> Dir.unlink "/")) ;
              equal string "EBUSY" (error_of (fun () -> Dir.rmdir "/")) ;
              equal string "EBUSY" (error_of (fun () -> Dir.rename "/" "/x")) ;
              equal string "EBUSY" (error_of (fun () -> Dir.rename "/d" "/")) ;
              equal (list string) ["d"] (Dir.readdir "/") ) ) ]

(* The edges of all domains. *)
let edges sim = Helpers.sum_stats sim (fun s -> s.S.edges)

let net =
  group "edges and names"
    [ test "a wait on an empty pipe gets no edge before the data" (fun () ->
          Sim.run (fun sim ->
              let r, w = Pipe.create () in
              let before = edges sim in
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      equal int 1 (Pipe.read r (Bytes.create 1) 0 1) ) ;
                  Fiber.yield () ;
                  equal int 0 (edges sim - before) ;
                  Pipe.write w "x" ) ;
              equal int 1 (edges sim - before) ;
              Pipe.close r ;
              Pipe.close w ) )
    ; test "extra_edges gives edges with no change" (fun () ->
          let idle = ref 0 in
          for seed = 1 to 10 do
            Sim.run ~seed (fun sim ->
                Sim.net_faults sim {Sim.no_faults with extra_edges= true} ;
                let r, w = Pipe.create () in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        ignore (Pipe.read r (Bytes.create 1) 0 1) ) ;
                    for _ = 1 to 50 do
                      Time.sleep Mono (Span.ms 1)
                    done ;
                    Pipe.write w "x" ) ;
                idle := !idle + Helpers.sum_stats sim (fun s -> s.S.idle_edges) ;
                Pipe.close r ;
                Pipe.close w )
          done ;
          greater int ~than:0 !idle )
    ; test "a listen after the close of a listener on the same address"
        (fun () ->
          Sim.run (fun sim ->
              let a = Sim.addr sim "again:1" in
              Net.close (Net.listen a) ;
              Net.close (Net.listen a) ) ) ]

let with_yield_bytes n f =
  let old = !Private.Net.yield_bytes in
  Private.Net.yield_bytes := n ;
  Fun.protect ~finally:(fun () -> Private.Net.yield_bytes := old) f

(* The switches of a ticker fiber while [reads] runs. The ticker runs only when
   the reader yields. *)
let ticks_during reads =
  let ticks = ref 0 in
  Fiber.scope (fun s ->
      Fiber.fork_daemon s (fun () ->
          while true do
            incr ticks ; Fiber.yield ()
          done ) ;
      Fiber.yield () ;
      let before = !ticks in
      reads () ; !ticks - before )

(* With a limit of 100 bytes, 100 reads of 10 bytes that always find data yield
   10 times, or 9 times if the reader yields one byte later. *)
let stream_reader_yields () =
  with_yield_bytes 100 (fun () ->
      Sim.run (fun sim ->
          let a, b = Sim.socket_pair sim in
          Net.write a (String.make 1000 'x') ;
          Time.sleep Mono (Span.ms 1) ;
          let buf = Bytes.create 10 in
          let n =
            ticks_during (fun () ->
                for _ = 1 to 100 do
                  equal int 10 (Net.read b buf 0 10)
                done )
          in
          at_least int ~than:9 n ; at_most int ~than:10 n ) )

(* An empty datagram counts as one byte: 50 empty datagrams with a limit of 10
   bytes yield 5 times, or 4 times if the reader yields one byte later. *)
let datagram_reader_yields () =
  with_yield_bytes 10 (fun () ->
      Sim.run (fun _ ->
          let u1 = Net.udp ("io", 2) and u2 = Net.udp ("io", 3) in
          let buf = Bytes.create 1 in
          for _ = 1 to 50 do
            Net.sendto u1 buf 0 0 ("io", 3)
          done ;
          Time.sleep Mono (Span.ms 1) ;
          let n =
            ticks_during (fun () ->
                for _ = 1 to 50 do
                  equal int 0 (fst (Net.recvfrom u2 buf 0 1))
                done )
          in
          at_least int ~than:4 n ; at_most int ~than:5 n ) )

let fair =
  group "fair reads"
    [ test "a reader of a stream that always finds data yields"
        stream_reader_yields
    ; test "a reader of datagrams yields, and an empty one counts as one byte"
        datagram_reader_yields ]

let tests = [files; net; fair]
