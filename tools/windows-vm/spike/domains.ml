(* The spike of the domains on Windows (plan step 1). Four domains each count to
   10 million with an Atomic and a Mutex, and a Condition wakes the main domain.
   Build in the VM: ocamlopt -I +unix unix.cmxa -o domains.exe domains.ml *)

let () =
  Printf.printf "recommended_domain_count: %d\n%!"
    (Domain.recommended_domain_count ()) ;
  let n = Atomic.make 0 and m = Mutex.create () and c = Condition.create () in
  let done_ = ref 0 in
  let t0 = Unix.gettimeofday () in
  let ds =
    List.init 4 (fun _ ->
        Domain.spawn (fun () ->
            for _ = 1 to 10_000_000 do
              Atomic.incr n
            done ;
            Mutex.protect m (fun () -> incr done_ ; Condition.signal c) ;
            (Domain.self () :> int) ) )
  in
  Mutex.protect m (fun () ->
      while !done_ < 4 do
        Condition.wait c m
      done ) ;
  let ids = List.map Domain.join ds in
  Printf.printf "domains %s: count %d in %.2f s\n"
    (String.concat ", " (List.map string_of_int ids))
    (Atomic.get n)
    (Unix.gettimeofday () -. t0) ;
  if Atomic.get n <> 40_000_000 then exit 1
