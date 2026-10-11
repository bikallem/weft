(* Child processes (design sections 2.3 and 2.7). The world gives a pidfd that
   is readable when the child exits. One fiber reaps the child, and the others
   read the status from an Ivar. *)

exception Error = Net.Error

type stdio = Op.stdio = Inherit | Null | Piped

type status = Op.status = Exited of int | Signaled of Signal.t

type t =
  { pid: int
  ; pidfd: Sched.fdrec
  ; stdin: Pipe.t option
  ; stdout: Pipe.t option
  ; stderr: Pipe.t option
  ; status: status Ivar.t
  ; reaping: bool Atomic.t  (** A fiber reaps the child. *) }

let spawn ?env ?cwd ?(stdin = Inherit) ?(stdout = Inherit) ?(stderr = Inherit)
    prog args =
  let sp = {Op.prog; argv= prog :: args; env; cwd; stdin; stdout; stderr} in
  match Net.submit "Process.spawn" (Op.Spawn sp) with
  | Ok r ->
      let alloc = Option.map Sched.Fdtab.alloc in
      { pid= r.pid
      ; pidfd= Sched.Fdtab.alloc r.pidfd
      ; stdin= alloc r.to_stdin
      ; stdout= alloc r.from_stdout
      ; stderr= alloc r.from_stderr
      ; status= Ivar.create ()
      ; reaping= Atomic.make false }
  | Error e ->
      raise (Error (e, "Process.spawn"))

let pid t = t.pid

let stdin t = t.stdin

let stdout t = t.stdout

let stderr t = t.stderr

(* The status after the exit. With [~clear], the last edge of the pidfd gave no
   exit, so the flag of the domain is stale. *)
let rec exited_ev ~clear t =
  Event.guard (fun () ->
      match Ivar.peek t.status with
      | Some s ->
          Event.always s
      | None ->
          Event.choose
            [ Ivar.read t.status
            ; Event.wrap (Net.fd_event ~write:false ~clear t.pidfd) (fun () ->
                  reap t ) ] )

(* The first fiber reaps the child, fills the status, and then closes the pidfd.
   The others wait for the status. *)
and reap t =
  if Atomic.compare_and_set t.reaping false true then (
    match Net.attempt t.pidfd "Process.wait" (fun os -> Op.Wait_child os) with
    | Ok (Some s) ->
        Ivar.fill t.status s ; Net.close t.pidfd ; s
    | Ok None ->
        Atomic.set t.reaping false ;
        Event.sync (exited_ev ~clear:true t)
    | Error e ->
        Atomic.set t.reaping false ;
        raise (Error (e, "Process.wait")) )
  else Event.sync (Ivar.read t.status)

let exited t = exited_ev ~clear:false t

let wait t = Event.sync (exited t)

(* A child that exited gets no signal. *)
let kill t s =
  match Ivar.peek t.status with
  | Some _ ->
      ()
  | None -> (
    match Net.attempt t.pidfd "Process.kill" (fun os -> Op.Kill (os, s)) with
    | Ok () ->
        ()
    | Error e ->
        raise (Error (e, "Process.kill")) )
