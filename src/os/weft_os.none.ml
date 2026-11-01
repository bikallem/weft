let run ?slack:_ ?domains:_ ?signals:_ ?watchdog:_ (_ : unit -> 'a) : 'a =
  failwith "Weft_os.run: no world of the OS on this system"
