(* The suite of weft-tls (design section 2.19), on the simulator. *)

let () =
  exit
    (Windtrap.run "tls"
       [ Test_flow_tls.tests
       ; Test_handshake.tests
       ; Test_read_rule.tests
       ; Test_upgrade.tests
       ; Test_configs.tests
       ; Test_shutdown.tests ] )
