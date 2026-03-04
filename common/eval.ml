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
    type step
  end) = struct

  type _ Effect.t +=
    | Enter : unit Effect.t
    | Step : X.step -> unit Effect.t

  let step st =
    Effect.perform @@ Step st
      
  let enter () =
    Effect.perform Enter

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

    type 'a t = {
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
    
    let ref v0 =
      let snapshot x = ref !x in
      mk ~snapshot (ref v0)
    
  end
    

  module Arg = struct

    type (_,_) t =
      | I : 'a -> ('a I.t -> 'x, 'x) t
      | M : 'a M.t -> ('a -> 'x, 'x) t

    type (_,_) list =
      | [] : ('a, 'a) list
      | (::) : ('a, 'b) t * ('b, 'c) list -> ('a, 'c) list

    type (_,_) r =
      | I : 'a ref -> ('a I.t -> 'x, 'x) r
      | M : 'a M.t -> ('a -> 'x, 'x) r
    
    type (_,_) refs =
      | [] : ('a, 'a) refs
      | (::) : ('a, 'b) r * ('b, 'c) refs -> ('a, 'c) refs

    let rec as_refs : type a x . (a, x) list -> (a, x) refs = function
      | [] -> []
      | I v :: t -> I (ref v) :: as_refs t
      | M v :: t -> M v :: as_refs t

    let rec as_values : type a x . (a, x) refs -> (a, x) list = function
      | [] -> []
      | I r :: t -> I !r :: as_values t
      | M ({m = (module M); v} as mv) :: t ->
        M {mv with v = M.snapshot v} :: as_values t

    let rec process_args
      : type a x . (a, x) refs -> a -> x
      = fun l run -> match l with
        | [] -> run
        | I r :: t -> process_args t (run @@ I.init r )
        | M mv :: t -> process_args t (run @@ mv.v )

  end

  let run f l =
    let refs = Arg.as_refs l in
    match Arg.process_args refs f with
    | c -> c
    | effect Step _, k ->
      Effect.Deep.continue k ()
    | effect Enter, k ->
      Effect.Deep.continue k ()

  let steps f l : _ Trace.t =
    let refs = Arg.as_refs l in
    fun () -> match Arg.process_args refs f with
      | c -> Return c
      | exception exn -> Error exn
      | effect Step c, k ->
        Trace.Cons ((c, Arg.as_values refs), Effect.Deep.continue k)
      | effect Enter, k ->
        Effect.Deep.continue k ()

end
