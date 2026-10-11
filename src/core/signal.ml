(* Signals as events (design section 2.7, Signals). *)

type t = Op.signal =
  | SIGHUP
  | SIGINT
  | SIGQUIT
  | SIGILL
  | SIGABRT
  | SIGFPE
  | SIGKILL
  | SIGSEGV
  | SIGBUS
  | SIGPIPE
  | SIGALRM
  | SIGTERM
  | SIGUSR1
  | SIGUSR2
  | SIGCHLD
  | SIGCONT
  | SIGSTOP
  | SIGTSTP
  | SIGWINCH
  | Other_signal of int

(* A signal from a CPU fault goes to the thread that caused it, and SIGKILL and
   SIGSTOP cannot be caught: they are not events. *)
let received s =
  Event.guard (fun () ->
      ( match s with
      | SIGILL | SIGFPE | SIGSEGV | SIGBUS | SIGKILL | SIGSTOP ->
          invalid_arg "Signal.received: this signal cannot be an event"
      | _ ->
          () ) ;
      (* The world gives the form of [s] that it delivers. *)
      let s =
        match Net.submit "Signal.received" (Op.Watch s) with
        | Ok s ->
            s
        | Error _ ->
            invalid_arg "Signal.received: the world does not watch this signal"
      in
      Event.Base
        (fun () ->
          let cell = ref 0 in
          {Event.leaf= Sched.signal_leaf s cell; result= (fun () -> !cell)} ) )
