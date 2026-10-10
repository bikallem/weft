module type ATOMIC = sig
  type !'a t

  val make : 'a -> 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val compare_and_set : 'a t -> 'a -> 'a -> bool
end

module Id = struct
  type gen = {domain: int; max_domains: int; mutable count: int}

  let gen ~domain ~max_domains =
    if max_domains < 1 then invalid_arg "Commit.Id.gen: max_domains < 1" ;
    if domain < 0 || domain >= max_domains then
      invalid_arg "Commit.Id.gen: domain out of range" ;
    {domain; max_domains; count= 0}

  let next g =
    let id = (g.count * g.max_domains) + g.domain in
    g.count <- g.count + 1 ;
    id
end

(* A claim holds a word for a short time, so a short spin is enough. *)
let max_spins = 256

let relax spins =
  for _ = 1 to spins do
    Domain.cpu_relax ()
  done ;
  Int.min max_spins (2 * spins)

module Make (A : ATOMIC) = struct
  type state = Waiting | Parked | Claimed | Done of int

  let pp_state ppf = function
    | Waiting ->
        Format.pp_print_string ppf "Waiting"
    | Parked ->
        Format.pp_print_string ppf "Parked"
    | Claimed ->
        Format.pp_print_string ppf "Claimed"
    | Done i ->
        Format.fprintf ppf "Done %d" i

  type t = {id: int; word: state A.t}

  let make id = {id; word= A.make Waiting}

  let id s = s.id

  let state s = A.get s.word

  let word s = s.word

  let self_commit s i =
    let rec loop spins =
      match A.get s.word with
      | Claimed ->
          loop (relax spins)
      | Done j ->
          Error j
      | Parked ->
          invalid_arg "Commit.self_commit: the sync is parked"
      | Waiting ->
          if A.compare_and_set s.word Waiting (Done i) then Ok ()
          else loop spins
    in
    loop 1

  let park s =
    let rec loop spins =
      match A.get s.word with
      | Claimed ->
          loop (relax spins)
      | Done i ->
          `Done i
      | Parked ->
          invalid_arg "Commit.park: the sync is parked"
      | Waiting ->
          if A.compare_and_set s.word Waiting Parked then `Parked
          else loop spins
    in
    loop 1

  type claim = Committed of {resume_partner: bool} | Stale | Self_done

  (* Change the word to [Claimed]. [Some old] is the value that the claim
     replaced, and [None] tells that the word is [Done]. *)
  let take s =
    let rec loop spins =
      match A.get s.word with
      | Claimed ->
          loop (relax spins)
      | Done _ ->
          None
      | (Waiting | Parked) as old ->
          if A.compare_and_set s.word old Claimed then Some old else loop spins
    in
    loop 1

  let claim mine i partner j ~on_commit =
    if mine == partner then invalid_arg "Commit.claim: own offer" ;
    if A.get mine.word = Parked then
      invalid_arg "Commit.claim: the sync is parked" ;
    let lo, hi =
      if
        ( mine.id < partner.id )
        [@mutate off "the ids of two syncs are never equal, so < and <= agree"]
      then (mine, partner)
      else (partner, mine)
    in
    let gone s = if s == partner then Stale else Self_done in
    match take lo with
    | None ->
        gone lo
    | Some old_lo -> (
      match take hi with
      | None ->
          A.set lo.word old_lo ; gone hi
      | Some old_hi ->
          ( match on_commit () with
          | () ->
              ()
          | exception e ->
              A.set hi.word old_hi ; A.set lo.word old_lo ; raise e ) ;
          A.set mine.word (Done i) ;
          A.set partner.word (Done j) ;
          let old_partner = if partner == lo then old_lo else old_hi in
          Committed {resume_partner= old_partner = Parked} )

  type notify = Resume | In_scan | Lost of int

  let notify s i =
    let rec loop spins =
      match A.get s.word with
      | Claimed ->
          loop (relax spins)
      | Done j ->
          Lost j
      | (Waiting | Parked) as old ->
          if A.compare_and_set s.word old (Done i) then
            if old = Parked then Resume else In_scan
          else loop spins
    in
    loop 1
end
