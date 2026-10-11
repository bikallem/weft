(* The CML events (design section 2.4). *)

open Sched

type 'a prim = {leaf: leaf; result: unit -> 'a}

type 'a t =
  | Base of (unit -> 'a prim)
  | Choose of 'a t list
  | Wrap : 'b t * ('b -> 'a) -> 'a t
  | Guard of (unit -> 'a t)
  | With_nack of (unit t -> 'a t)

let choose l = Choose l

let wrap e f = Wrap (e, f)

let guard g = Guard g

let with_nack f = With_nack f

let never = Choose []

let always v =
  Base
    (fun () ->
      { leaf= (fun _ s i -> ignore (C.self_commit s.word i))
      ; result= (fun () -> v) } )

let of_ivar iv =
  Base (fun () -> {leaf= Iv.leaf iv; result= (fun () -> Iv.get iv)})

(* A nack of [with_nack]: an Ivar, and the range of the branches that it
   covers. *)
type nack = {niv: unit ivar; first: int; mutable last: int}

(* Run the guards depth first, and make the branches. *)
let flatten (type a) (ev : a t) =
  let leaves = ref [] and results = ref [] and nacks = ref [] and n = ref 0 in
  let rec go : type b. b t -> (b -> a) -> unit =
   fun ev k ->
    match ev with
    | Base mk ->
        let p = mk () in
        leaves := p.leaf :: !leaves ;
        results := (fun () -> k (p.result ())) :: !results ;
        incr n
    | Choose l ->
        List.iter (fun e -> go e k) l
    | Wrap (e, f) ->
        go e (fun x -> k (f x))
    | Guard g ->
        go (g ()) k
    | With_nack f ->
        let nk = {niv= Iv.make (); first= !n; last= max_int} in
        nacks := nk :: !nacks ;
        go (f (of_ivar nk.niv)) k ;
        nk.last <- !n
  in
  match go ev Fun.id with
  | () ->
      ( Array.of_list (List.rev !leaves)
      , Array.of_list (List.rev !results)
      , !nacks )
  | exception e ->
      (* A guard raised: enable the nacks of the guards that ran before. *)
      let bt = Printexc.get_raw_backtrace () in
      List.iter (fun nk -> ignore (fill_resume nk.niv ())) !nacks ;
      Printexc.raise_with_backtrace e bt

(* Enable each nack whose branches did not commit. *)
let enable nacks i =
  List.iter
    (fun nk ->
      if i < nk.first || i >= nk.last then ignore (fill_resume nk.niv ()) )
    nacks

(* A base event, the most frequent form, needs no flatten (M3). *)
let sync_with ~nocancel ev =
  match ev with
  | Base mk ->
      let p = mk () in
      let i = sync_req ~nocancel [|p.leaf|] in
      if i < 0 then raise (Cancelled (reason ())) else p.result ()
  | _ ->
      let leaves, results, nacks = flatten ev in
      let i = sync_req ~nocancel leaves in
      enable nacks i ;
      if i < 0 then raise (Cancelled (reason ())) else results.(i) ()

let sync ev = sync_with ~nocancel:false ev

(* [Fiber.yield]: the fiber goes to the tail of its queue. It is a cancellation
   point. *)
let yield () = if prog "Fiber.yield" Yield then raise (Cancelled (reason ()))
