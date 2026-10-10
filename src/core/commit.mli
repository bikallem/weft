(** The commit protocol of a [sync] (design section 2.4).

    This module has no effects. It does not resume a fiber. Each function tells
    the caller which fiber it must resume, if one. Thus the scheduler keeps the
    continuations, and [dscheck] can examine this module with its own atomic
    locations (milestone M1). [spec/tla/Commit.tla] is a model of the same
    steps. *)

(** The atomic operations that the protocol uses. [Stdlib.Atomic] satisfies this
    signature. *)
module type ATOMIC = sig
  type !'a t

  val make : 'a -> 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val compare_and_set : 'a t -> 'a -> 'a -> bool
end

(** The ids of syncs. Each domain gives ids from its own counter, so no counter
    is shared between domains. The ids are unique and totally ordered. *)
module Id : sig
  (** The id counter of one domain. Only that domain uses it. *)
  type gen

  val gen : domain:int -> max_domains:int -> gen
  (** [gen ~domain ~max_domains] is the counter of domain [domain].

      @raise Invalid_argument
        if [max_domains < 1] or [domain] is not in the range 0 to
        [max_domains - 1]. *)

  val next : gen -> int
  (** [next g] is a new id: the local count × [max_domains] + [domain]. The
      local count starts at 0. *)
end

module Make (A : ATOMIC) : sig
  (** The state word of a sync. *)
  type state =
    | Waiting  (** The fiber is in its scan. *)
    | Parked  (** The fiber ended its scan and waits for a partner. *)
    | Claimed  (** A claimer holds the word for a short time. *)
    | Done of int  (** The sync committed to this branch. *)

  val pp_state : Format.formatter -> state -> unit
  (** [pp_state] prints a state as its constructor, for example [Done 2]. *)

  (** A sync: an id and a state word. *)
  type t

  val make : int -> t
  (** [make id] is a new sync in the state [Waiting]. [id] comes from
      {!Id.next}. *)

  val id : t -> int
  (** The id of the sync. *)

  val state : t -> state
  (** The state word. A read only. *)

  val word : t -> state A.t
  (** The atomic location of the state word. Only tests and [Sched_debug] use
      it. *)

  (** {1 The fiber of the sync}

      The fiber that syncs calls these functions during its scan, and only that
      fiber. *)

  val self_commit : t -> int -> (unit, int) result
  (** [self_commit s i] commits a ready branch [i] that has no partner, for
      example [always] or a full [Ivar]. It changes the word from [Waiting] to
      [Done i]. It gives [Error j] if another side committed the sync first to
      branch [j], for example a cancel. If the word is [Claimed], it waits until
      the word changes.

      @raise Invalid_argument if the sync is [Parked]. *)

  val park : t -> [`Parked | `Done of int]
  (** [park s] ends the scan. If the word is [Waiting], it changes the word to
      [Parked] and gives [`Parked]: the fiber is suspended, and the side that
      later commits the sync resumes it. If the word is [Done i], a partner
      committed during the scan: the fiber continues with branch [i]. If the
      word is [Claimed], it waits until the word changes.

      @raise Invalid_argument if the sync is [Parked]. *)

  (** The result of a claim of two words. *)
  type claim =
    | Committed of {resume_partner: bool}
        (** The two syncs committed. The caller resumes the partner only if
            [resume_partner] is [true]. If it is [false], the partner is in its
            scan, and its scan finds [Done]. *)
    | Stale
        (** The word of the partner is [Done]: its offer is stale. The caller
            removes the offer and looks for another partner. *)
    | Self_done
        (** The word of the fiber is [Done]: a partner committed this sync
            during the scan. The caller stops the scan. *)

  val claim : t -> int -> t -> int -> on_commit:(unit -> unit) -> claim
  (** [claim mine i partner j ~on_commit] commits branch [i] of [mine] with
      branch [j] of [partner]. The fiber of [mine] calls it during its scan, and
      [partner] is the sync of an offer that it found.

      It changes the two words to [Claimed], the word with the lower id first.
      If a word is [Claimed], it waits until the word changes. This wait is
      safe, because the claimer holds only words with a lower id. When it holds
      the two words, it calls [on_commit], then it sets the words to [Done i]
      and [Done j]. [on_commit] moves the value of a channel, before the partner
      can see [Done]. If [on_commit] raises, the claim restores the two words
      and raises the exception again.

      @raise Invalid_argument
        if [mine] and [partner] are the same sync, or if [mine] is [Parked]. *)

  (** {1 One-word partners} *)

  (** The result of {!notify}. *)
  type notify =
    | Resume  (** The sync was [Parked]. The caller resumes the fiber. *)
    | In_scan  (** The sync was [Waiting]. Its scan finds [Done]. *)
    | Lost of int  (** The sync committed first to this branch. *)

  val notify : t -> int -> notify
  (** [notify s i] commits branch [i] of [s] for a side that changes only the
      word of [s]: a cancel, a timer, an fd event, [Ivar.fill] or a nack. If the
      word is [Claimed], it waits until the word changes. It never drops the
      event because the word is [Claimed]. *)
end
