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
  type ('state, 'v) node =
    | Cons of 'state * ('state, 'v) t
    | Return of 'v
    | Error of exn
  and ('state, 'v) t = unit -> ('state, 'v) node      

  let rec as_seq k () = match k () with
    | Cons (x, next) ->
      Seq.Cons (`Step x, as_seq next)
    | Return v -> Seq.Cons (`Ret v, Seq.empty)
    | Error exn -> Seq.Cons (`Error exn, Seq.empty)

  let rec pp pp_elt pp_end fmt k =
    match k () with
    | Cons (x, next) ->
      pp_elt fmt x;
      pp pp_elt pp_end fmt next
    | Return v ->
      pp_end fmt v
    | Error exn ->
      Format.pp_print_string fmt (Printexc.to_string exn)
end


module I = struct

  type 'a t = C : {
      root : 'b ref;
      lens : ('b, 'a) Lens.t;
      (* view : 'a ; *)
    } -> 'a t

  let init x = C { root = x ; lens = Lens.id }

  let view (C c) = c.lens.get !(c.root)

  let sub (C c) l _v =
    let c' = C { c with lens = Lens.compose l c.lens} in
    (* assert (view c' = v); *)
    c'

  let map f (C c) =
    c.root := Lens.modify c.lens f !(c.root);
    C c

  let set c x = map (fun _ -> x) c

  let list c = 
    List.mapi (fun i view -> sub c (Lens.for_list i) view) (view c)

end

module M = struct 

  module type S = sig
    type t
    val snapshot : t -> t
  end

  type 'a state = {
    m : (module S with type t = 'a);
    v : 'a
  }

  let pp (ppf : 'a Fmt.t) fmt mv = ppf fmt mv.v

  let mk (type a) ~snapshot v =
    let module M = struct
      type t = a
      let snapshot = snapshot
    end in
    { m = (module M) ; v}

end

module Conf = struct

  type 'a mut = 'a
  type 'a imm = 'a I.t

  module Val = struct

    type (_,_) one =
      | I : 'a -> ('a imm -> 'x, 'x) one
      | M : 'a -> ('a mut -> 'x, 'x) one

    type (_,_) t =
      | [] : ('a, 'a) t
      | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

  end

  module State = struct

    type (_,_) one =
      | I : 'a ref -> ('a imm -> 'x, 'x) one
      | M : 'a M.state -> ('a mut -> 'x, 'x) one

    type (_,_) t =
      | [] : ('a, 'a) t
      | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

    let rec run
      : type a x . (a, x) t -> a -> x
      = fun l f -> match l with
        | [] -> f
        | I r :: t -> run t (f @@ I.init r )
        | M mv :: t -> run t (f @@ mv.v )

    let rec snapshot : type a x . (a, x) t -> (a, x) Val.t = function
      | [] -> []
      | I r :: t -> I !r :: snapshot t
      | M {m = (module M); v} :: t ->
        M (M.snapshot v) :: snapshot t

  end

  include Val

end

module Arg = struct
  type (_,_) one =
    | I : 'a -> ('a Conf.imm -> 'x, 'x) one
    | M : 'a M.state -> ('a Conf.mut -> 'x, 'x) one

  type (_,_) t =
    | [] : ('a, 'a) t
    | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

  let i x = I x
  let m ~snapshot x = M (M.mk ~snapshot x)

  let ref v0 =
    let snapshot x = ref !x in
    m ~snapshot (ref v0)

  let rec conf : type a x . (a, x) t -> (a, x) Conf.State.t = function
    | [] -> []
    | I v :: t -> I (Stdlib.ref v) :: conf t
    | M v :: t -> M v :: conf t
end

module Make (X : sig
    type step
  end) = struct

  type _ Effect.t +=
    | Enter : unit Effect.t
    | Step : X.step -> unit Effect.t

  let step st =
    Effect.perform @@ Step st
      
  let enter () =
    Effect.perform Enter


  type ('a, 'x) trace = (X.step * ('a, 'x) Conf.t, 'x) Trace.t

  let run f l =
    let conf = Arg.conf l in
    match Conf.State.run conf f with
    | c -> c
    | effect Step _, k ->
      Effect.Deep.continue k ()
    | effect Enter, k ->
      Effect.Deep.continue k ()

  let trace f l : _ Trace.t =
    let conf = Arg.conf l in
    fun () -> match Conf.State.run conf f with
      | c -> Return c
      | exception exn -> Error exn
      | effect Step c, k ->
        Trace.Cons ((c, Conf.State.snapshot conf), Effect.Deep.continue k)
      | effect Enter, k ->
        Effect.Deep.continue k ()

end
