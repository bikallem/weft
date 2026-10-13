(* The resource errors on Linux (issue 3). The test runs this program with a
   small limit of open fds (prlimit --nofile). The program fills the free fds,
   then each operation that makes an fd must give EMFILE. The accept of a
   connection with no free fd gives EMFILE, and the connection stays in the
   backlog. It prints "ok" at the end. *)

open Weft

let emfile name f =
  match f () with
  | _ ->
      failwith (name ^ ": no error")
  | exception Net.Error (Net.No_resources "EMFILE", _) ->
      ()

let () =
  Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
      let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
      let a = Net.local_addr l in
      let rec fill acc =
        match Net.udp ("127.0.0.1", 0) with
        | u ->
            fill (u :: acc)
        | exception Net.Error (Net.No_resources "EMFILE", _) ->
            acc
      in
      let spare = fill [] in
      emfile "Net.udp" (fun () -> Net.udp ("127.0.0.1", 0)) ;
      emfile "Net.listen" (fun () -> Net.listen (Net.Tcp ("127.0.0.1", 0))) ;
      emfile "Net.connect" (fun () -> Net.connect a) ;
      emfile "Net.accept" (fun () -> Net.accept l) ;
      emfile "Pipe.create" Pipe.create ;
      emfile "File.openfile" (fun () -> File.openfile [Read] "/dev/null") ;
      emfile "Dir.readdir" (fun () -> Dir.readdir "/") ;
      emfile "Process.spawn" (fun () -> Process.spawn ~stdout:Piped "true" []) ;
      match spare with
      | u :: v :: rest ->
          Net.close u ;
          let c = Net.connect a in
          Net.write c "hi" ;
          emfile "Net.accept" (fun () -> Net.accept l) ;
          Net.close v ;
          let s, _ = Net.accept l in
          let buf = Bytes.create 2 in
          let k = Net.read s buf 0 2 in
          if Bytes.sub_string buf 0 k <> "hi" then failwith "no data" ;
          List.iter Net.close (s :: c :: l :: rest)
      | _ ->
          failwith "less than two free fds" ) ;
  print_endline "ok"
