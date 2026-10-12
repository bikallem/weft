(* The jobs of the pool: the [Blocking] operations (design sections 2.3 and 2.5,
   Blocking operations). A fiber starts a job and waits for its completion. On a
   cancel, it detaches from the job and raises [Cancelled] at once. *)

open Sched

let wait id =
  Event.sync
    (Event.Base (fun () -> {Event.leaf= external_leaf id; result= ignore}))

(* Run [job]. With [~fd], the job holds a user of the fd until it completes,
   also after a detach: thus the world never closes the fd while the pool uses
   it. At the end of [run], a domain waits for such a job. *)
(* The paths and the names that go to the OS. *)
let strings : type a. a Op.blocking -> string list = function
  | Op.Open (path, _, _) ->
      [path]
  | Op.Stat path ->
      [path]
  | Op.Mkdir (path, _) ->
      [path]
  | Op.Readdir path ->
      [path]
  | Op.Rename (src, dst) ->
      [src; dst]
  | Op.Unlink path ->
      [path]
  | Op.Rmdir path ->
      [path]
  | Op.Resolve host ->
      [host]
  | Op.Reverse addr ->
      [addr]
  | Op.Pread _ | Op.Pwrite _ | Op.Fsync _ | Op.Fstat _ | Op.Ftruncate _ ->
      []

let run ?fd name job =
  (* The fiber does not start a job in a cancelled context. *)
  check_cancel () ;
  List.iter (Net.check_string name) (strings job) ;
  Option.iter (fun fd -> Net.acquire fd name) fd ;
  let on_done = Option.map (fun fd () -> Rc.release fd) fd in
  match prog name (Job (job, on_done)) with
  | Error e ->
      Option.iter Rc.release fd ;
      raise (Net.Error (e, name))
  | Ok id ->
      ( match wait id with
      | () ->
          ()
      | exception (Cancelled _ as e) ->
          let bt = Printexc.get_raw_backtrace () in
          prog name (Detach_job id) ;
          Printexc.raise_with_backtrace e bt ) ;
      Net.ok name (Net.submit name (Op.Finish (id, job)))
