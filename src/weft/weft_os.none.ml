(* No world of the OS on this system. *)

let run ?slack:_ ?domains:_ ?signals:_ ?watchdog:_ (_ : unit -> 'a) : 'a =
  failwith "Weft.run: no world of the OS on this system"
