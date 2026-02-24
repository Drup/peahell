module Expr (E : sig
    type expr
    type context
    val plug : outter:context -> inner:context -> context
    val pp_expr : Format.formatter -> expr -> unit
    val pp_context : Format.formatter -> context -> unit
  end) = struct
  open E
  
  type t = { ctx: E.context ; view : E.expr }

  let mk ctx view = { ctx; view }
  
  let inside outter { ctx; view }=
    let ctx = E.plug ~outter ~inner:ctx in
    { ctx; view }
  
  module Infix = struct
    let (^>) ctx v = mk ctx v
    let (^>>) ctx e = inside ctx e
  end
  include Infix
   
  let view e = e.view
  let ctx e = e.ctx

  let pp fmt {ctx;view} =
    Fmt.pf fmt "@[@[%a@] ▷@ @[%a@]@]" pp_context ctx pp_expr view
end

module Trace = struct
  type ('a, 'b) node =
    | Cons of 'a * ('a, 'b) t
    | Return of 'b
    | Error of exn
  and ('a, 'b) t = unit -> ('a, 'b) node      

  let rec as_seq k () = match k () with
    | Cons (x, next) ->
      Seq.Cons (`Step x, as_seq next)
    | Return v -> Seq.Cons (`Ret v, Seq.empty)
    | Error exn -> Seq.Cons (`Error exn, Seq.empty)

  let rec pp ?(pp_sep=Fmt.cut) pp_elt pp_end fmt k =
    match k () with
    | Cons (x, next) ->
      pp_elt fmt x; pp_sep fmt ();
      pp ~pp_sep pp_elt pp_end fmt next
    | Return v ->
      pp_end fmt v
    | Error exn ->
      Format.pp_print_string fmt (Printexc.to_string exn)
end

module Make (X : sig
    type state
    type configuration
    type value
  end) = struct

  include X

  type _ Effect.t +=
    | Update : (state -> state) -> state Effect.t
    | Enter : configuration -> unit Effect.t
    | Exit : value -> unit Effect.t
    | Yield : configuration -> unit Effect.t          

  let get () =
    Effect.perform @@ Update (fun st -> st)

  let set st =
    ignore @@ Effect.perform @@ Update (fun _ -> st)

  let update up =
    Effect.perform @@ Update up

  let yield c = Effect.perform @@ Yield c
  let recurse f (x : configuration) =
    Effect.perform @@ Enter x;
    let v = f x in
    Effect.perform @@ Exit v;
    v

  let run f st0 c0 : _ Seq.t =
    let st : state ref = ref st0 in
    match f c0 with
    | c -> c
    | effect Yield _, k ->
      Effect.Deep.continue k ()
    | effect Enter _, k ->
      Effect.Deep.continue k ()
    | effect Exit _, k ->
      Effect.Deep.continue k ()
    | effect Update up, k ->
      st := up !st;
      Effect.Deep.continue k !st

  let steps f ~state c : _ Trace.t =
    let st : state ref = ref state in
    fun () -> match f c with
      | c -> Return (!st, c)
      | exception exn -> Error exn
      | effect Yield c, k ->
        Trace.Cons ((!st, c), Effect.Deep.continue k)
      | effect Enter c, k ->
        Trace.Cons ((!st, c), Effect.Deep.continue k)
      | effect Exit _v, k ->
        Effect.Deep.continue k ()
      | effect Update up, k ->
        st := up !st;
        Effect.Deep.continue k !st

end
