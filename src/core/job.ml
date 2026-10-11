(* The jobs of the pool: the [Blocking] operations (design sections 2.3 and 2.5,
   Blocking operations). A fiber starts a job and waits for its completion. On a
   cancel, it detaches from the job and raises [Cancelled] at once. *)

open Sched

let wait id =
  Event.sync
    (Event.Base (fun () -> {Event.leaf= external_leaf id; result= ignore}))

(* The fiber does not start a job in a cancelled context. *)
let check_cancel () =
  match first_flag (List.hd (self ()).ctxs) with
  | Some r ->
      raise (Cancelled r)
  | None ->
      ()

(* Run [job]. With [~fd], the job holds a user of the fd until it completes,
   also after a detach: thus the world never closes the fd while the pool uses
   it. *)
let run ?fd name job =
  check_cancel () ;
  Option.iter (fun fd -> Net.acquire fd name) fd ;
  let on_done () = Option.iter Rc.release fd in
  match prog name (Job (job, on_done)) with
  | Error e ->
      on_done () ;
      raise (Net.Error (e, name))
  | Ok id -> (
      ( match wait id with
      | () ->
          ()
      | exception (Cancelled _ as e) ->
          let bt = Printexc.get_raw_backtrace () in
          prog name (Detach_job id) ;
          Printexc.raise_with_backtrace e bt ) ;
      match World.perform name (World.Submit (Op.Finish (id, job))) with
      | Ok v ->
          v
      | Error e ->
          raise (Net.Error (e, name)) )
