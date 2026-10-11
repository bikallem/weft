(* The tests of the library layers on the simulator (design section 2.14,
   M2). *)

open Weft
module Sim = Weft_sim

(* Each test of the fiber layer runs with 200 seeds on 4 virtual domains. *)
let seeds_200 =
  let n = ref 0 in
  { Portable.name= "200 seeds"
  ; domains= 4
  ; run=
      (fun f ->
        let rec go seed =
          let v = Sim.run ~seed ~domains:4 (fun _ -> f ()) in
          if seed = 200 then v else go (seed + 1)
        in
        go 1 )
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("m2", !n) in
        (Net.listen a, a) ) }

let tests = [Windtrap.group "200 seeds" [Portable.layers seeds_200]]
