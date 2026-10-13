(* A file job in a pool that cannot start a thread, then a cancel of its fiber
   before the completion comes. The test runs this program with a limit of one
   process (prlimit --nproc=1:1), so pthread_create fails. The review of the
   limit of detached jobs found a crash here. Also, a job and a spawn give
   [No_resources "EAGAIN"] (issue 3). It prints "ok" at the end. *)

open Weft

let eagain name f =
  match f () with
  | _ ->
      failwith (name ^ ": no error")
  | exception Net.Error (Net.No_resources "EAGAIN", _) ->
      ()

let () =
  Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
      eagain "File.stat" (fun () -> File.stat "/") ;
      eagain "Process.spawn" (fun () -> Process.spawn "true" []) ;
      match
        Fiber.scope (fun s ->
            Fiber.fork s (fun () -> ignore (File.stat "/")) ;
            Fiber.fork s (fun () -> failwith "boom") )
      with
      | () ->
          ()
      | exception Failure _ ->
          () ) ;
  print_endline "ok"
