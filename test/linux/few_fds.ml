(* The resource errors on Linux (issue 3). The test runs this program with a
   small limit of open fds (prlimit --nofile). The program fills the free fds,
   then each operation that makes an fd must give EMFILE. The accept of a
   connection with no free fd gives EMFILE, and the connection stays in the
   backlog.

   Then a create with signals fails at each fd that it opens: the signalfd, and
   the epoll set, the eventfd and the two timerfds of domain 0. After each
   failure, the world closed the fds that it made and restored the signal mask.
   A later world with signals works. The program prints "ok" at the end. *)

open Weft

let emfile name f =
  match f () with
  | _ ->
      failwith (name ^ ": no error")
  | exception Net.Error (Net.No_resources "EMFILE", _) ->
      ()

(* [true] if the calling thread blocks SIGUSR1 (10 on Linux). One free fd is
   necessary. *)
let blocks_usr1 () =
  let lines =
    In_channel.with_open_text "/proc/thread-self/status" In_channel.input_lines
  in
  let l = List.find (String.starts_with ~prefix:"SigBlk:") lines in
  let mask =
    Int64.of_string ("0x" ^ String.trim (String.sub l 7 (String.length l - 7)))
  in
  Int64.logand mask (Int64.shift_left 1L 9) <> 0L

(* [create ~signals] must fail with EMFILE in the call [name]. *)
let create_fails name =
  match Weft_linux.create ~signals:[SIGUSR1] () with
  | w ->
      Weft_linux.close w ;
      failwith (name ^ ": no error")
  | exception Failure m when m = name ^ ": Too many open files" ->
      ()

let fill () =
  let rec go acc =
    match Net.udp ("127.0.0.1", 0) with
    | u ->
        go (u :: acc)
    | exception Net.Error (Net.No_resources "EMFILE", _) ->
        acc
  in
  go []

(* With [k] free fds, a create with signals fails in the call [name]. After the
   failure, the [k] fds are free again, and SIGUSR1 is not blocked. *)
let fails_with_free spare k name =
  let rec free k l =
    if k = 0 then l
    else
      match l with
      | u :: l ->
          Net.close u ;
          free (k - 1) l
      | [] ->
          failwith "no fd"
  in
  let spare = free k spare in
  create_fails name ;
  let again = fill () in
  if List.length again <> k then
    failwith
      (Printf.sprintf "%s: %d fds stay open" name (k - List.length again)) ;
  (* Free one fd to read the mask. *)
  let spare =
    match again @ spare with
    | u :: l ->
        Net.close u ; l
    | [] ->
        failwith "no fd"
  in
  if blocks_usr1 () then failwith (name ^ ": SIGUSR1 stays blocked") ;
  fill () @ spare

let failed_create () =
  Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
      (* The fds that [create] opens, in order: the signalfd, then for domain 0
         epoll, eventfd and two timerfds. *)
      let spare =
        List.fold_left
          (fun spare (k, name) -> fails_with_free spare k name)
          (fill ())
          [ (0, "signalfd")
          ; (1, "epoll_create1")
          ; (2, "eventfd")
          ; (3, "timerfd_create")
          ; (4, "timerfd_create") ]
      in
      List.iter Net.close spare ;
      Weft_linux.close (Weft_linux.create ~signals:[SIGUSR1] ()) ;
      if blocks_usr1 () then failwith "SIGUSR1 is blocked after the close" )

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
  failed_create () ;
  print_endline "ok"
