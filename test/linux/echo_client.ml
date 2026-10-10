(* A client in another process, with blocking Unix calls: it sends "hello" and
   prints what comes back. *)
let () =
  let port = int_of_string Sys.argv.(1) in
  let s = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.connect s (ADDR_INET (Unix.inet_addr_loopback, port)) ;
  ignore (Unix.write_substring s "hello" 0 5) ;
  let buf = Bytes.create 5 in
  let rec loop off =
    if off < 5 then
      match Unix.read s buf off (5 - off) with 0 -> off | n -> loop (off + n)
    else off
  in
  let n = loop 0 in
  print_string (Bytes.sub_string buf 0 n) ;
  Unix.close s
