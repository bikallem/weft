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
  let r = Net.ok "Process.spawn" (Net.submit "Process.spawn" (Op.Spawn sp)) in
  let alloc kind = Option.map (Sched.Fdtab.alloc ~kind) in
  { pid= r.pid
  ; pidfd= Sched.Fdtab.alloc r.pidfd
  ; stdin= alloc Write_end r.to_stdin
  ; stdout= alloc Read_end r.from_stdout
  ; stderr= alloc Read_end r.from_stderr
  ; status= Ivar.create ()
  ; reaping= Atomic.make false }

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
   The others also watch the pidfd, because the reaper can leave with no status
   (after a cancel, or an edge with no exit). Their yield lets the reaper go
   on. *)
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
  else (
    Event.yield () ;
    Event.sync (exited_ev ~clear:false t) )

let exited t = exited_ev ~clear:false t

let wait t = Event.sync (exited t)

(* A child that exited gets no signal. A reap on another domain can come between
   the read of the status and the send. Then the error of the send (ESRCH, or
   EBADF after the close of the pidfd) is not an error. *)
let kill t s =
  let reaped () = Atomic.get t.reaping || Option.is_some (Ivar.peek t.status) in
  if Option.is_none (Ivar.peek t.status) then
    match Net.attempt t.pidfd "Process.kill" (fun os -> Op.Kill (os, s)) with
    | Ok () ->
        ()
    | Error _ when reaped () ->
        ()
    | Error e ->
        raise (Error (e, "Process.kill"))
    | exception Error (Op.Bad_fd, _) when reaped () ->
        ()
