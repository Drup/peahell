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

module Tree = struct

  type ('state, 'v) node =
    | Cons of 'state * ('state, 'v) t
    | Return of 'v
    | Error of exn
    | Choice of ('state, 'v) t list
  and ('state, 'v) t = unit -> ('state, 'v) node   
  
  let rec pp pp_elt pp_end fmt k =
    match k () with
    | Cons (x, next) ->
      pp_elt fmt x;
      pp pp_elt pp_end fmt next
    | Return v ->
      pp_end fmt v
    | Error exn ->
      Fmt.exn fmt exn
    | Choice l ->
      Fmt.pf fmt "@[<v 2>Choice [@,%a@]@,]"
        (Fmt.list @@ pp pp_elt pp_end) l
end

module I = struct

  type accessor_kind = Accessor.optional
  
  type 'a t = C : {
      root : 'b ref;
      lens : (unit, 'a, 'b, accessor_kind) Accessor.t;
      view : 'a;
    } -> 'a t

  let init x = C { root = x ; view = !x ; lens = Accessor.id }

  let view (C c) = c.view

  exception Invalid_subterm
            : 'b t * (unit, 'a, 'b, accessor_kind) Accessor.t -> exn
  
  let sub (C c) l v0 =
    let view = match Accessor.get_option l c.view with
      | None -> raise @@ Invalid_subterm (C c, l)
      | Some v -> v
    in
    let c' = C { c with view; lens = Accessor.compose c.lens l} in
    assert (view == v0);
    c'

  let map f (C c) =
    let v' = f c.view in
    c.root := Accessor.set c.lens ~to_:v' !(c.root);
    C {c with view = v'}

  let set c x = map (fun _ -> x) c

  let list c =
    let rec set_nth i l elt = match i, l with
      | _, [] -> []
      | 0, _h :: t -> elt :: t
      | n, h :: t -> h :: set_nth (n-1) t elt
    in
    let a i =
      let set = set_nth i in
      let match_ l = match List.nth_opt l i with
        | Some v -> Base.Either.First v
        | None -> Base.Either.Second l
      in
      Accessor.optional ~match_ ~set
    in
    List.mapi (fun i v -> sub c (a i) v) (view c)

end

module NI = struct 

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
      | M : 'a NI.state -> ('a mut -> 'x, 'x) one

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
    | M : 'a NI.state -> ('a Conf.mut -> 'x, 'x) one

  type (_,_) t =
    | [] : ('a, 'a) t
    | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

  let i x = I x
  let m ~snapshot x = M (NI.mk ~snapshot x)

  let ref v0 =
    let snapshot x = ref !x in
    m ~snapshot (ref v0)

  let pure v =
    let snapshot x = x in
    m ~snapshot v 

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
    | Choice : int -> int Effect.t

  let step st =
    Effect.perform @@ Step st
      
  let enter () =
    Effect.perform Enter

  module Choice = struct

    let int n = Effect.perform (Choice n)
    let one_of fs =
      let l = List.length fs in
      let k = int l in
      List.nth fs k

    type 'a task = Todo of (unit -> 'a) | Done of 'a
    let rec all_done = function
      | [] -> `Yes []
      | (k, Done v) :: t ->
        begin match all_done t with
          | `Yes l -> `Yes ((k, v) :: l)
          | `No l -> `No l
        end
      | (k, Todo f) :: t ->
        begin match all_done t with
          | `Yes _ -> `No [k, f]
          | `No l -> `No ((k, f) :: l)
        end
    let tasks_assoc l =
      let tasks = List.map (fun (k, x) -> k, Todo x) l in
      let rec go tasks = match all_done tasks with
        | `Yes vs -> vs
        | `No todos ->
          let k, f = one_of todos in
          let tasks =
            List.map
              (fun (k', v) -> k', (if k = k' then Done (f ()) else v))
              tasks
          in
          go tasks
      in
      go tasks

    let tasks l =
      List.map snd @@ tasks_assoc @@ List.mapi (fun i x -> i, x) l
    let map f l =
      List.map snd @@ tasks_assoc @@ List.mapi (fun i x -> i, fun () -> f x) l
        
    let pair f1 f2 =
      let[@warning "-8"] [v1, v2] = tasks [f1; f2] in
      v1, v2

    let (|||) = pair

  end

  type ('a, 'x) trace = (X.step * ('a, 'x) Conf.t, 'x) Trace.t

  module S = Effect.Shallow
  
  let run ?(random=Random.State.make_self_init ()) f l =
    let conf = Arg.conf l in
    let rec go : type a . (a, _) S.continuation -> a -> _
      = fun k x ->
        S.continue_with k x {
          retc = Fun.id;
          exnc = raise;
          effc = fun (type b) (eff : b Effect.t) ->
            match eff with
            | Step _ -> Some (fun (k : (b, _) S.continuation) ->
                go k ()
              )
            | Enter -> Some (fun (k : (b, _) S.continuation) ->
                go k ()
              )
            | Choice n -> Some (fun (k : (b, _) S.continuation) ->
                go k (Random.State.int random n)
              )
            | _ -> None
        }
    in
    go (S.fiber @@ Conf.State.run conf) f


  let trace ?(random=Random.State.make_self_init ()) f l : _ Trace.t =
    let conf = Arg.conf l in
    let retc x = Trace.Return x in
    let exnc err = Trace.Error err in
    let rec go : type a . (a, _) S.continuation -> a -> _
      = fun k x () ->
        S.continue_with k x {
          retc; exnc;
          effc = fun (type b) (eff : b Effect.t) ->
            match eff with
            | Step c -> Some (fun (k : (b, _) S.continuation) ->
                Trace.Cons (
                  (c, Conf.State.snapshot conf),
                  go k ())
              )
            | Enter -> Some (fun (k : (b, _) S.continuation) ->
                go k () ()
              )
            | Choice n -> Some (fun (k : (b, _) S.continuation) ->
                go k (Random.State.int random n) ()
              )
            | _ -> None
        }
    in
    go (S.fiber @@ Conf.State.run conf) f

  module MS = Multicont.Shallow

  let tree f l : _ Tree.t =
    let conf = Arg.conf l in
    let retc x = Tree.Return x in
    let exnc err = Tree.Error err in
    let rec go : type a . (a, _) MS.resumption -> a -> _
      = fun k x () ->
        MS.resume_with k x {
          retc; exnc;
          effc = fun (type b) (eff : b Effect.t) ->
            match eff with
            | Step c -> Some (fun (k : (b, _) S.continuation) ->
                let k = MS.promote k in
                Tree.Cons (
                  (c, Conf.State.snapshot conf),
                  go k ())
              )
            | Enter -> Some (fun (k : (b, _) S.continuation) ->
                let k = MS.promote k in
                go k () ()
              )
            | Choice n -> Some (fun (k : (b, _) S.continuation) ->
                let k = Multicont.Shallow.promote k in
                Tree.Choice (List.init n (fun v -> go k v))
              )
            | _ -> None
        }
    in
    go (MS.promote @@ S.fiber @@ Conf.State.run conf) f
  
end
