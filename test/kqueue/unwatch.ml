(* The program of the unwatch tests on macOS and FreeBSD
   (test/portable/unwatch_main.ml). *)
let () = Unwatch_main.main ~run:Weft_kqueue.run
