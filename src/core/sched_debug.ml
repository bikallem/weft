(* A read of the state of the scheduler for the checks of the simulator (design
   section 2.8, The wait steps). It changes nothing. *)

open Sched

(* The name of a fiber: its own, or "fiber-<id>". *)
let name f = if f.name = "" then "fiber-" ^ string_of_int f.fid else f.name

let fibers t =
  Spin.with_lock t.fibers_lock (fun () ->
      Hashtbl.fold (fun _ f acc -> f :: acc) t.fibers [] )
  |> List.sort (fun a b -> compare a.fid b.fid)

(* The fibers that wait in a sync that did not commit. *)
let blocked t =
  List.filter_map
    (fun f ->
      match Atomic.get f.current with
      | Some s when not (is_done s) ->
          Some (name f)
      | _ ->
          None )
    (fibers t)

(* The parked syncs, by the name of the fiber. A test of the checks uses them to
   plant a lost wakeup. *)
let parked t =
  List.filter_map
    (fun f ->
      match Atomic.get f.current with
      | Some s when C.state s.word = C.Parked ->
          Some (name f, s)
      | _ ->
          None )
    (fibers t)

(* The items of the check for lost wakeups. Each one is a problem. *)
let lost t ~now =
  let now = Clock.to_ns Mono now in
  let queues =
    Array.to_list t.doms
    |> List.concat_map (fun d ->
        ( if Ring.is_empty d.ring && Option.is_none (Atomic.get d.next) then []
          else [Printf.sprintf "the queue of domain %d is not empty" d.index] )
        @
        match Atomic.get d.mailbox with
        | [] ->
            []
        | _ :: _ ->
            [Printf.sprintf "domain %d has close notices" d.index] )
  in
  let queues =
    if Spin.with_lock t.overflow_lock (fun () -> Queue.is_empty t.overflow) then
      queues
    else "the overflow queue is not empty" :: queues
  in
  let timers =
    Array.to_list t.doms
    |> List.concat_map (fun d ->
        List.init d.heap.size (fun i -> d.heap.entries.(i))
        |> List.filter_map (fun e ->
            if
              (not (is_done e.esync))
              && ( Int64.compare e.deadline now
                 <= 0 )
                 [@mutate
                   off
                     "the scheduler expires each entry up to now before a \
                      sleep, so an entry at now is a bug in expire, not in \
                      this check"]
            then
              Some
                (Printf.sprintf "%s has an expired timer" (name e.esync.owner))
            else None ) )
  in
  let done_not_queued =
    List.filter_map
      (fun f ->
        match Atomic.get f.current with
        | Some s when is_done s && not f.queued ->
            Some (Printf.sprintf "%s is committed but not in a queue" (name f))
        | _ ->
            None )
      (fibers t)
  in
  queues @ timers @ done_not_queued

(* The heap entries whose sync did not commit. *)
let live_timers t =
  Array.fold_left
    (fun n d ->
      let live = ref 0 in
      for i = 0 to d.heap.size - 1 do
        if not (is_done d.heap.entries.(i).esync) then incr live
      done ;
      n + !live )
    0 t.doms

(* The live waiters of each registered fd: the fd of the world, and the names of
   the fibers that wait to read and to write. *)
let fd_waiters t =
  let names w =
    List.filter_map
      (fun (s, _) -> if is_done s then None else Some (name s.owner))
      (Waiters.to_list w)
  in
  Array.to_list t.doms
  |> List.concat_map (fun d ->
      Hashtbl.fold
        (fun _ reg acc -> (reg.fdr.os, names reg.rwait, names reg.wwait) :: acc)
        d.regs [] )
  |> List.sort compare
