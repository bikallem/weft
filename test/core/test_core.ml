let () =
  exit
    (Windtrap.run "core"
       (Test_commit.tests @ Test_world.tests @ Test_heap.tests) )
