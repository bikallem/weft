(* The walk in the file system of the simulator, on 2 virtual domains, for 30
   seeds. Each File or Dir operation takes a random time of 0 to 1 ms. *)

open Weft
module Sim = Weft_sim

let write path size =
  let f = File.openfile [Write; Create] path in
  File.write f (String.make size 'x') ;
  File.close f

let () =
  for seed = 1 to 30 do
    Sim.run ~seed ~domains:2 (fun sim ->
        Sim.set_blocking_cost sim (Span.ms 1) ;
        List.iter Dir.mkdir
          ["/data"; "/data/sub"; "/data/sub/deep"; "/data/empty"; "/data/wide"] ;
        write "/data/a" 10 ;
        write "/data/b" 20 ;
        write "/data/sub/c" 30 ;
        write "/data/sub/deep/d" 40 ;
        for i = 1 to 20 do
          write (Printf.sprintf "/data/wide/%02d" i) 5
        done ;
        let s = Du.du ~limit:4 "/data" in
        assert (s.files = 24 && s.dirs = 5 && s.bytes = 200 && s.errors = 0) ;
        (* 20 fibers want to stat the files of "/data/wide", but only 4 can run
           at the same time. *)
        assert (s.peak = 4) ;
        let s = Du.du ~limit:1 "/data" in
        assert (s.bytes = 200 && s.peak = 1) ;
        (* A root that does not exist is an error, not an exception. *)
        let s = Du.du "/nothing" in
        assert (s.files = 0 && s.errors = 1) )
  done
