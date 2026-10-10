open Windtrap

let skeleton =
  group "skeleton"
    [ test "the version is 0.0.0" (fun () -> equal string "0.0.0" Weft.version)
    ; test "the world is sim" (fun () -> equal string "sim" Weft_sim.name) ]

let () = exit (run "sim" [skeleton])
