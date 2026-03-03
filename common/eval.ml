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
    type step
    type configuration
  end) = struct

  include X

  type 'a expr = { view : 'a ; lens : (configuration, 'a) Lens.t }
  let view x = x.view
  let sub e l view = { view; lens = Lens.compose l e.lens}
  
  type _ Effect.t +=
    | Update : (state -> state) -> state Effect.t
    | Swap : 'a expr -> unit Effect.t
    | Enter : unit Effect.t
    | Step : step -> unit Effect.t          

  let get () =
    Effect.perform @@ Update (fun st -> st)

  let set st =
    ignore @@ Effect.perform @@ Update (fun _ -> st)

  let update up =
    Effect.perform @@ Update up

  let step st =
    Effect.perform @@ Step st
      
  let enter () =
    Effect.perform Enter

  let swap e =
    Effect.perform @@ Swap e

  let map f {view;lens} =
    let e = {lens; view = f view} in
    Effect.perform (Swap e);
    e
  
  let run f ~state:st0 e0=
    let st : state ref = ref st0 in
    let e : configuration ref = ref e0 in
    match f {view = e0; lens = Lens.id} with
    | c -> c
    | effect Step _, k ->
      Effect.Deep.continue k ()
    | effect Enter, k ->
      Effect.Deep.continue k ()
    | effect Swap {view; lens}, k ->
      e := lens.set view !e;
      Effect.Deep.continue k ()
    | effect Update up, k ->
      st := up !st;
      Effect.Deep.continue k !st

  let steps f ~state:st0 e0 : _ Trace.t =
    let st : state ref = ref st0 in
    let e : configuration ref = ref e0 in
    fun () -> match f {view = e0; lens = Lens.id} with
      | c -> Return (!st, c)
      | exception exn -> Error exn
      | effect Step c, k ->
        Trace.Cons ((!st, !e, c), Effect.Deep.continue k)
      | effect Enter, k ->
        Effect.Deep.continue k ()
      | effect Swap {view; lens}, k ->
        e := lens.set view !e;
        Effect.Deep.continue k ()
      | effect Update up, k ->
        st := up !st;
        Effect.Deep.continue k !st

  module List = struct

    let map f {view; lens} =
      List.mapi (fun i view ->
          let lens = Lens.compose (Lens.for_list i) lens in
          f {view; lens}
        )
        view

  end
end
