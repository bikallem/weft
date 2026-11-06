(** The world of the OS that the build runs on: [Weft_linux] on Linux,
    [Weft_kqueue] on macOS and FreeBSD, and [Weft_windows] on Windows. A
    portable program runs [Weft.run (module Weft_os) main].

    On another system, [run] raises [Failure]. Thus a program that uses the
    simulator only can link this library on each system. *)

include Weft.OS
