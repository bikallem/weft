open Windtrap

let skeleton =
  group "skeleton"
    [test "the world is linux" (fun () -> equal string "linux" Weft_linux.name)]

let () = exit (run "linux" [skeleton])
