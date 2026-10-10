(* Promises (design section 2.5, Promises). *)

type 'a t = ('a, exn * Printexc.raw_backtrace) result Ivar.t

let fork ?name scope f =
  let p = Ivar.create () in
  Fiber.fork ?name scope (fun () ->
      let r =
        match Fiber.scope (fun _ -> f ()) with
        | v ->
            Ok v
        | exception e ->
            Error (e, Printexc.get_raw_backtrace ())
      in
      Ivar.fill p r ) ;
  p

let ready p =
  Event.wrap (Ivar.read p) (function
    | Ok v ->
        v
    | Error (e, bt) ->
        Printexc.raise_with_backtrace e bt )

let await p = Event.sync (ready p)
