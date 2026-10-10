(* The tests of the commit protocol (design section 2.4). The scenarios "cross",
   "stale" and "self" are the scenarios of spec/tla/Commit.tla. *)

open Windtrap
module Commit = Weft.Private.Commit
module Id = Commit.Id
module C = Commit.Make (Atomic)

let state = Testable.make ~pp:C.pp_state ~equal:( = )

let pp_claim ppf = function
  | C.Committed {resume_partner} ->
      Format.fprintf ppf "Committed { resume_partner = %b }" resume_partner
  | C.Stale ->
      Format.pp_print_string ppf "Stale"
  | C.Self_done ->
      Format.pp_print_string ppf "Self_done"

let claim = Testable.make ~pp:pp_claim ~equal:( = )

let pp_notify ppf = function
  | C.Resume ->
      Format.pp_print_string ppf "Resume"
  | C.In_scan ->
      Format.pp_print_string ppf "In_scan"
  | C.Lost i ->
      Format.fprintf ppf "Lost %d" i

let notify = Testable.make ~pp:pp_notify ~equal:( = )

let pp_park ppf = function
  | `Parked ->
      Format.pp_print_string ppf "`Parked"
  | `Done i ->
      Format.fprintf ppf "`Done %d" i

let park = Testable.make ~pp:pp_park ~equal:( = )

let self = result unit int

let nothing () = ()

(* The ids. *)

let ids_of ~max_domains picks =
  let gens =
    Array.init max_domains (fun domain -> Id.gen ~domain ~max_domains)
  in
  List.map (fun d -> (d, Id.next gens.(d))) picks

let ids_are_unique (max_domains, picks) =
  let ids = List.map snd (ids_of ~max_domains picks) in
  equal int (List.length ids) (List.length (List.sort_uniq compare ids))

let id_gives_its_domain (max_domains, picks) =
  List.iter
    (fun (d, id) -> equal int d (id mod max_domains))
    (ids_of ~max_domains picks)

let ids_increase (max_domains, picks) =
  let last = Array.make max_domains (-1) in
  List.iter
    (fun (d, id) ->
      greater int ~than:last.(d) id ;
      last.(d) <- id )
    (ids_of ~max_domains picks)

let picks =
  let open Gen in
  let* max_domains = int_range 1 8 in
  let+ picks = list (int_range 0 (max_domains - 1)) in
  (max_domains, picks)

let picks =
  Gen.with_pp
    (fun ppf (m, l) ->
      Format.fprintf ppf "(%d, [%s])" m
        (String.concat "; " (List.map string_of_int l)) )
    picks

let ids =
  group "ids"
    [ prop "two ids are never equal" picks ids_are_unique
    ; prop "an id modulo max_domains is its domain" picks id_gives_its_domain
    ; prop "the ids of one domain increase" picks ids_increase
    ; test "the first ids of domain 1 of 4 are 1, 5 and 9" (fun () ->
          let g = Id.gen ~domain:1 ~max_domains:4 in
          let a = Id.next g in
          let b = Id.next g in
          let c = Id.next g in
          equal (list int) [1; 5; 9] [a; b; c] )
    ; test "one domain is permitted" (fun () ->
          let g = Id.gen ~domain:0 ~max_domains:1 in
          let a = Id.next g in
          let b = Id.next g in
          equal (list int) [0; 1] [a; b] )
    ; cases "a bad gen raises Invalid_argument"
        ~name:(fun (d, m) -> Printf.sprintf "domain %d of %d" d m)
        [(0, 0); (-1, 4); (4, 4); (min_int, 4); (max_int, 4)]
        (fun (domain, max_domains) ->
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> Id.gen ~domain ~max_domains) ) ]

(* The scenarios of the model. *)

(* "cross": Y parks with its offer, then X finds the offer and claims. X resumes
   Y, because Y was parked. *)
let cross_parked () =
  let x = C.make 0 and y = C.make 1 in
  equal park `Parked (C.park y) ;
  let moved = ref 0 in
  equal claim
    (C.Committed {resume_partner= true})
    (C.claim x 0 y 1 ~on_commit:(fun () -> incr moved)) ;
  equal int 1 !moved ;
  equal state (C.Done 0) (C.state x) ;
  equal state (C.Done 1) (C.state y) ;
  equal park (`Done 0) (C.park x)

(* "cross" with the higher id first: the claimer holds the lower id first, but
   the result is the same. *)
let cross_higher_id () =
  let x = C.make 7 and y = C.make 2 in
  equal park `Parked (C.park y) ;
  equal claim
    (C.Committed {resume_partner= true})
    (C.claim x 1 y 0 ~on_commit:nothing) ;
  equal state (C.Done 1) (C.state x) ;
  equal state (C.Done 0) (C.state y)

(* Y is still in its scan: X commits, but does not resume Y. The scan of Y finds
   Done. *)
let cross_in_scan () =
  let x = C.make 0 and y = C.make 1 in
  equal claim
    (C.Committed {resume_partner= false})
    (C.claim x 0 y 1 ~on_commit:nothing) ;
  equal park (`Done 1) (C.park y)

(* "stale": the timer of Y fired, so the offer of Y is stale. *)
let stale_partner () =
  let x = C.make 0 and y = C.make 1 in
  equal park `Parked (C.park y) ;
  equal notify C.Resume (C.notify y 5) ;
  let moved = ref 0 in
  equal claim C.Stale (C.claim x 0 y 1 ~on_commit:(fun () -> incr moved)) ;
  equal int 0 !moved ;
  equal state C.Waiting (C.state x) ;
  equal state (C.Done 5) (C.state y)

(* "stale" with the partner as the higher id: the claim restores the word of
   X. *)
let stale_partner_hi () =
  let x = C.make 0 and y = C.make 1 in
  equal notify C.In_scan (C.notify y 5) ;
  equal claim C.Stale (C.claim x 0 y 1 ~on_commit:nothing) ;
  equal state C.Waiting (C.state x)

(* A partner committed X during its scan. *)
let self_done id_x =
  let x = C.make id_x and y = C.make 1 in
  equal notify C.In_scan (C.notify x 3) ;
  equal claim C.Self_done (C.claim x 0 y 1 ~on_commit:nothing) ;
  equal state C.Waiting (C.state y) ;
  equal park (`Done 3) (C.park x)

(* "self": a ready branch and a cancel. The first one wins. *)
let self_then_cancel () =
  let x = C.make 0 in
  equal self (Ok ()) (C.self_commit x 2) ;
  equal notify (C.Lost 2) (C.notify x 9) ;
  equal park (`Done 2) (C.park x)

let cancel_then_self () =
  let x = C.make 0 in
  equal notify C.In_scan (C.notify x 9) ;
  equal self (Error 9) (C.self_commit x 2) ;
  equal park (`Done 9) (C.park x)

let cancel_parked () =
  let x = C.make 0 in
  equal park `Parked (C.park x) ;
  equal notify C.Resume (C.notify x 9) ;
  equal notify (C.Lost 9) (C.notify x 4)

let on_commit_raises () =
  let x = C.make 0 and y = C.make 1 in
  equal park `Parked (C.park y) ;
  raises Exit (fun () -> C.claim x 0 y 1 ~on_commit:(fun () -> raise Exit)) ;
  equal state C.Waiting (C.state x) ;
  equal state C.Parked (C.state y)

let misuse =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  [ test "a claim of its own offer raises Invalid_argument" (fun () ->
        let x = C.make 0 in
        raises_match invalid (fun () -> C.claim x 0 x 1 ~on_commit:nothing) ;
        equal state C.Waiting (C.state x) )
  ; test "a claim by a parked sync raises Invalid_argument" (fun () ->
        let x = C.make 0 and y = C.make 1 in
        ignore (C.park x) ;
        raises_match invalid (fun () -> C.claim x 0 y 1 ~on_commit:nothing) ;
        equal state C.Waiting (C.state y) )
  ; test "a self-commit of a parked sync raises Invalid_argument" (fun () ->
        let x = C.make 0 in
        ignore (C.park x) ;
        raises_match invalid (fun () -> C.self_commit x 0) )
  ; test "a second park raises Invalid_argument" (fun () ->
        let x = C.make 0 in
        ignore (C.park x) ;
        raises_match invalid (fun () -> C.park x) ) ]

let scenarios =
  group "scenarios"
    ( [ test "a sync keeps its id" (fun () -> equal int 7 (C.id (C.make 7)))
      ; test "cross: X resumes the parked Y" cross_parked
      ; test "cross: the claim of a lower id gives the same result"
          cross_higher_id
      ; test "cross: X does not resume Y in its scan" cross_in_scan
      ; test "stale: a fired timer makes the offer stale" stale_partner
      ; test "stale: the claim restores the word of the claimer"
          stale_partner_hi
      ; test "a partner committed the claimer (lower id)" (fun () ->
            self_done 0 )
      ; test "a partner committed the claimer (higher id)" (fun () ->
            self_done 2 )
      ; test "self: a ready branch wins over a later cancel" self_then_cancel
      ; test "self: a cancel wins over a later ready branch" cancel_then_self
      ; test "a cancel resumes a parked sync one time" cancel_parked
      ; test "a raise in on_commit restores the two words" on_commit_raises ]
    @ misuse )

(* The trace of "cross", as text. *)
let cross_trace () =
  let x = C.make 0 and y = C.make 1 in
  let show step =
    Format.printf "%-22s x: %a, y: %a@." step C.pp_state (C.state x) C.pp_state
      (C.state y)
  in
  show "start" ;
  ignore (C.park y) ;
  show "y parks" ;
  let r = C.claim x 0 y 1 ~on_commit:(fun () -> show "x holds the two words") in
  show (Format.asprintf "x claims: %a" pp_claim r) ;
  ignore (C.park x) ;
  show "x ends its scan" ;
  expect (output ())
  @@ __POS_OF__
       {|
    start                  x: Waiting, y: Waiting
    y parks                x: Waiting, y: Parked
    x holds the two words  x: Claimed, y: Claimed
    x claims: Committed { resume_partner = true } x: Done 0, y: Done 1
    x ends its scan        x: Done 0, y: Done 1
    |}

let trace = group "trace" [test "the states of cross" cross_trace]

(* The retry paths on Claimed. One domain never sees a Claimed word of another
   side, so a hooked atomic changes the word between two operations. *)

module Hooked = struct
  type 'a t = 'a Atomic.t

  let before : (unit -> unit) list ref = ref []

  let step () =
    match !before with
    | [] ->
        ()
    | f :: rest ->
        before := rest ;
        f ()

  let make = Atomic.make

  let get c = step () ; Atomic.get c

  let set = Atomic.set

  let compare_and_set c a b =
    step () ;
    Atomic.compare_and_set c a b
end

module H = Commit.Make (Hooked)

let hstate = Testable.make ~pp:H.pp_state ~equal:( = )

(* [with_steps s actions f] sets the word of [s], then runs [f] with one action
   before each get or compare-and-set. [None] is no action. *)
let with_steps s first actions f =
  Atomic.set (H.word s) first ;
  Hooked.before :=
    List.map
      (function None -> ignore | Some st -> fun () -> Atomic.set (H.word s) st)
      actions ;
  Fun.protect ~finally:(fun () -> Hooked.before := []) f

let retry =
  group "retry on Claimed"
    [ test "notify waits on Claimed, then resumes the parked sync" (fun () ->
          let s = H.make 0 in
          with_steps s H.Claimed [None; Some H.Parked] (fun () ->
              is_true (H.notify s 4 = H.Resume) ) ;
          equal hstate (H.Done 4) (H.state s) )
    ; test "notify waits on Claimed and does not drop the event" (fun () ->
          let s = H.make 0 in
          with_steps s H.Claimed [None; None; Some H.Waiting] (fun () ->
              is_true (H.notify s 4 = H.In_scan) ) )
    ; test "notify loses a race to Done between the get and the CAS" (fun () ->
          let s = H.make 0 in
          with_steps s H.Waiting [None; Some (H.Done 1)] (fun () ->
              is_true (H.notify s 4 = H.Lost 1) ) )
    ; test "self_commit waits on Claimed" (fun () ->
          let s = H.make 0 in
          with_steps s H.Claimed [None; Some H.Waiting] (fun () ->
              equal self (Ok ()) (H.self_commit s 2) ) )
    ; test "self_commit loses a race to Done between the get and the CAS"
        (fun () ->
          let s = H.make 0 in
          with_steps s H.Waiting [None; Some (H.Done 1)] (fun () ->
              equal self (Error 1) (H.self_commit s 2) ) )
    ; test "park waits on Claimed" (fun () ->
          let s = H.make 0 in
          with_steps s H.Claimed [None; Some H.Waiting] (fun () ->
              is_true (H.park s = `Parked) ) )
    ; test "park loses a race to Done between the get and the CAS" (fun () ->
          let s = H.make 0 in
          with_steps s H.Waiting [None; Some (H.Done 1)] (fun () ->
              is_true (H.park s = `Done 1) ) )
    ; test "a claim waits on a Claimed partner, then commits" (fun () ->
          let x = H.make 0 and y = H.make 1 in
          (* The gets: the check of x, x (Waiting), y (Claimed), y again. *)
          with_steps y H.Claimed [None; None; None; None; Some H.Parked]
            (fun () ->
              is_true
                ( H.claim x 0 y 1 ~on_commit:nothing
                = H.Committed {resume_partner= true} ) ) ;
          equal hstate (H.Done 0) (H.state x) ;
          equal hstate (H.Done 1) (H.state y) )
    ; test "a claim loses a race to Done on the partner" (fun () ->
          let x = H.make 0 and y = H.make 1 in
          (* The CAS of y fails: y is Done before it. *)
          with_steps y H.Waiting [None; None; None; None; Some (H.Done 3)]
            (fun () -> is_true (H.claim x 0 y 1 ~on_commit:nothing = H.Stale) ) ;
          equal hstate H.Waiting (H.state x) ;
          equal hstate (H.Done 3) (H.state y) ) ]

(* The stateful test: the system against a sequential model. The model never has
   Claimed, because on one domain each call ends before the next one starts. *)

type phase = Scan | Suspended | Resumed

let pp_phase ppf p =
  Format.pp_print_string ppf
    ( match p with
    | Scan ->
        "scan"
    | Suspended ->
        "suspended"
    | Resumed ->
        "resumed" )

type model = {mid: int; mutable st: C.state; mutable mphase: phase}

type sys =
  {s: C.t; mutable phase: phase; mutable resumes: int; mutable moved: int}

let gen = Id.gen ~domain:0 ~max_domains:1

let last_id = ref 0

(* The fiber of a sync continues: after a park with Done, or after a resume. *)
let resume s =
  s.resumes <- s.resumes + 1 ;
  s.phase <- Resumed

let invariant m s =
  equal state m.st (C.state s.s) ;
  at_most int ~than:1 s.resumes ;
  (* No lost wakeup: a suspended fiber has a parked sync. *)
  if s.phase = Suspended then equal state C.Parked (C.state s.s) ;
  if s.phase = Resumed then equal int 1 s.resumes ;
  if s.phase = Scan then equal int 0 s.resumes

let sync =
  abstract "s" ~invariant ~pp:(fun ppf m ->
      Format.fprintf ppf "%d: %a, %a" m.mid C.pp_state m.st pp_phase m.mphase )

let branch = Gen.int_range 0 3

let m_make () = {mid= !last_id; st= C.Waiting; mphase= Scan}

let s_make () =
  let id = Id.next gen in
  last_id := id ;
  {s= C.make id; phase= Scan; resumes= 0; moved= 0}

let m_self m i =
  match m.st with
  | C.Done j ->
      Error j
  | _ ->
      m.st <- C.Done i ;
      Ok ()

let s_self s i = C.self_commit s.s i

let m_park m =
  match m.st with
  | C.Done i ->
      m.mphase <- Resumed ;
      `Done i
  | _ ->
      m.st <- C.Parked ;
      m.mphase <- Suspended ;
      `Parked

let s_park s =
  let r = C.park s.s in
  (match r with `Done _ -> resume s | `Parked -> s.phase <- Suspended) ;
  r

let m_claim mine i partner j =
  let lo, hi =
    if mine.mid < partner.mid then (mine, partner) else (partner, mine)
  in
  let gone m = if m == partner then C.Stale else C.Self_done in
  match (lo.st, hi.st) with
  | C.Done _, _ ->
      gone lo
  | _, C.Done _ ->
      gone hi
  | _ ->
      let parked = partner.st = C.Parked in
      mine.st <- C.Done i ;
      partner.st <- C.Done j ;
      if parked then partner.mphase <- Resumed ;
      C.Committed {resume_partner= parked}

let s_claim mine i partner j =
  let r =
    C.claim mine.s i partner.s j ~on_commit:(fun () ->
        mine.moved <- mine.moved + 1 )
  in
  (match r with C.Committed {resume_partner= true} -> resume partner | _ -> ()) ;
  (* on_commit runs exactly when the claim commits. *)
  equal int (match r with C.Committed _ -> 1 | _ -> 0) mine.moved ;
  mine.moved <- 0 ;
  r

let m_notify m i =
  match m.st with
  | C.Done j ->
      C.Lost j
  | C.Parked ->
      m.st <- C.Done i ;
      m.mphase <- Resumed ;
      C.Resume
  | _ ->
      m.st <- C.Done i ;
      C.In_scan

let s_notify s i =
  let r = C.notify s.s i in
  if r = C.Resume then resume s ;
  r

let in_scan m = m.mphase = Scan

let commands =
  [ command "make" (Gen.unit @-> makes sync) m_make s_make
  ; command "self_commit"
      ~pre:(fun m _ -> in_scan m)
      (sync ^-> branch @-> returns self)
      m_self s_self
  ; command "park" ~pre:in_scan (sync ^-> returns park) m_park s_park
  ; command "claim"
      ~pre:(fun mine _ partner _ -> in_scan mine && mine != partner)
      (sync ^-> branch @-> sync ^-> branch @-> returns claim)
      m_claim s_claim
  ; command "notify" (sync ^-> branch @-> returns notify) m_notify s_notify
  ; command "state"
      (sync ^-> returns state)
      (fun m -> m.st)
      (fun s -> C.state s.s) ]

let model =
  group "model"
    [ stateful "the protocol behaves like the sequential model" ~count:500
        commands ]

let tests = [ids; scenarios; trace; retry; model]
