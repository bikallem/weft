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
let check name s =
  match s with
  | SIGILL | SIGFPE | SIGSEGV | SIGBUS | SIGKILL | SIGSTOP ->
      invalid_arg (name ^ ": this signal cannot be an event")
  | _ ->
      ()

(* Raise the error of a signal that the world does not watch. *)
let not_watched name =
  invalid_arg (name ^ ": the world does not watch this signal")

let received s =
  Event.guard (fun () ->
      check "Signal.received" s ;
      (* The world gives the form of [s] that it delivers. *)
      let s =
        match Net.submit "Signal.received" (Op.Watch s) with
        | Ok (Some s) ->
            s
        | Ok None ->
            not_watched "Signal.received"
        | Error e ->
            raise (Net.Error (e, "Signal.received"))
      in
      Event.Base
        (fun () ->
          let cell = ref 0 in
          { Event.leaf= Sched.signal_leaf s cell
          ; result=
              (fun () ->
                (* -1: an unwatch ended the watch while the fiber waited. *)
                if
                  ( !cell < 0 )
                  [@mutate off "a commit gives -1 or a count of 1 or more"]
                then not_watched "Signal.received"
                else !cell ) } ) )

(* The world stops the watch first, so a signal that it reads later is not lost:
   [deliver] sends it again (design section 2.7, The end of a watch). *)
let unwatch s =
  check "Signal.unwatch" s ;
  let s =
    match Net.submit "Signal.unwatch" (Op.Unwatch s) with
    | Ok (Some s) ->
        s
    | Ok None ->
        not_watched "Signal.unwatch"
    | Error e ->
        raise (Net.Error (e, "Signal.unwatch"))
  in
  let n = Sched.prog "Signal.unwatch" (Sched.Unwatch_signal s) in
  if n > 0 then ignore (Net.submit "Signal.unwatch" (Op.Resend s))
