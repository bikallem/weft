(* The tests of the I/O of the simulator: the files, and the edges and the names
   of the network (design sections 2.6 and 2.7). *)

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
              equal (list string) ["x"] (Dir.readdir "/d") ) ) ]

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

let tests = [files; net]
