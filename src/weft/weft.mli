(** Weft: fibers, CML events and pluggable worlds.

    This module is {!Weft_core} with {!run}, which runs a program with the world
    of the OS. The build selects the world: [Weft_linux] on Linux, and
    [Weft_kqueue] on macOS and FreeBSD. *)

include module type of struct
  include Weft_core
end

val run :
     ?slack:Span.t
  -> ?domains:int
  -> ?signals:Signal.t list
  -> ?watchdog:Span.t option
  -> (unit -> 'a)
  -> 'a
(** [run main] runs [main] with a new world of the OS, and closes the world at
    the end. It is [Weft_linux.run] on Linux, and [Weft_kqueue.run] on macOS and
    FreeBSD. The defaults are not the same as those of {!run_world}: [~domains]
    is [min 4 (Domain.recommended_domain_count ())] by default (open question
    6), and [~slack] is 100 us by default (open question 7).

    [~signals] (none by default) are the signals that [Signal.received] can wait
    for. The worlds of the OS tell how they get them.

    [~watchdog] is the limit of the watchdog, 10 ms by default (open question
    7): if a fiber runs on a domain for more than the limit with no switch, a C
    thread writes a warning to [stderr] with the index of the domain. [None]
    stops the watchdog.

    @raise Failure on a system with no world of the OS. *)
