
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
  
  type 'a t = C : {
      root : 'b ref;
      lens : ('b, 'b, 'a, 'a) Lun.t;
      view : 'a;
    } -> 'a t

  let id () = Lun.lense Fun.id (fun _ x -> x)
  let init x = C { root = x ; view = !x ; lens = id }

  let view (C c) = c.view

  exception Invalid_subterm
            : 'b t * ('b, 'b, 'a, 'a) Lun.t -> exn
  
  let sub (C c) l v0 =
    let view = match Lun.get_opt l c.view with
      | None -> raise @@ Invalid_subterm (C c, l)
      | Some v -> v
    in
    let c' = C { c with view; lens = Lun.(c.lens >> l)} in
    assert (view == v0);
    c'

  let map f (C c) =
    let v' = f c.view in
    c.root := Lun.set c.lens v' !(c.root);
    C {c with view = v'}

  let set c x = map (fun _ -> x) c

  let rec set_nth i l elt = match i, l with
    | _, [] -> []
    | 0, _h :: t -> elt :: t
    | n, h :: t -> h :: set_nth (n-1) t elt
  let list_nth i () = Lun.lense (fun l -> List.nth l i) (set_nth i)
  let list c =
    List.mapi (fun i v -> sub c (list_nth i) v) (view c)

end

module NI = struct 

  type 'st t = NI : {
    v : 'st ;
    snapshot : 'st -> 'st ;
    save : 'st -> 'copy ;
    restore : 'st -> 'copy -> unit ;
  } -> 'st t

  (* let pp (ppf : 'a Fmt.t) fmt mv = ppf fmt mv.v *)

  let mk ~snapshot ~restore ~save v = NI {
      snapshot ;
      restore ;
      save ;
      v ;
    }

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
      | M : 'st NI.t -> ('st mut -> 'x, 'x) one

    type (_,_) t =
      | [] : ('a, 'a) t
      | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

    let rec run
      : type a x . (a, x) t -> a -> x
      = fun l f -> match l with
        | [] -> f
        | I r :: t -> run t (f @@ I.init r )
        | M NI mv :: t -> run t (f @@ mv.v )

    let rec snapshot : type a x . (a, x) t -> (a, x) Val.t = function
      | [] -> []
      | I r :: t -> I !r :: snapshot t
      | M NI mv :: t ->
        M (mv.snapshot mv.v) :: snapshot t

  end

  module Backtrack = struct
    type 'st copy = C : {
        copy : 'copy ;
        restore : 'st -> 'copy -> unit ;
      } -> 'st copy

    type (_,_) one =
      | I : 'a -> ('a imm -> 'x, 'x) one
      | M : 'st copy -> ('st mut -> 'x, 'x) one

    type (_,_) t =
      | [] : ('a, 'a) t
      | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t

    let rec checkpoint : type a x . (a, x) State.t -> (a, x) t = function
      | [] -> []
      | I r :: t -> I !r :: checkpoint t
      | M NI {save; restore; v; _} :: t ->
        M (C {restore ; copy = save v} ) :: checkpoint t

    let rec restore : type a x . (a, x) State.t -> (a, x) t -> unit =
      fun st copy -> match st, copy with
        | [], [] -> ()
        | I r :: t, I v :: t' -> r := v; restore t t'
        | M NI ni :: t, M C mc :: t' ->
          mc.restore ni.v mc.copy; restore t t'
        | _, _ -> assert false (* TODO explain to the typechecker. *)

    
  end

  include Val

end

module Arg = struct
  type (_) one =
    | I : 'a -> ('a Conf.imm) one
    | M : 'a NI.t -> ('a Conf.mut) one

  type (_,_) t =
    | [] : ('a, 'a) t
    | (::) : ('a) one * ('b, 'c) t -> ('a -> 'b, 'c) t

  let i x = I x
  let m ~snapshot ~save ~restore x = M (NI.mk ~snapshot ~save ~restore x)

  let ref v0 =
    let snapshot x = ref !x in
    let save x = !x in
    let restore r x = r := x in
    m ~snapshot ~save ~restore (ref v0)

  (* Todo make the array immutable. *)
  let array a0 =
    let snapshot x = Array.copy x in
    let save x = Array.copy x in
    let restore a v =
      assert (Array.length a = Array.length v);
      Array.blit v 0 a 0 (Array.length a)
    in
    m ~snapshot ~save ~restore a0

  let hashtbl h0 =
    let snapshot h = Hashtbl.copy h in
    let save h = List.of_seq @@ Hashtbl.to_seq h in
    let restore h l =
      Hashtbl.filter_map_inplace
        (fun k _ -> List.assq_opt k l) h
    in
    m ~snapshot ~save ~restore h0

  let pure v =
    let snapshot x = x in
    let save x = x and restore _ _ = () in
    m ~snapshot ~save ~restore v 

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
    let map_nd f l =
      List.map snd @@ tasks_assoc @@ List.mapi (fun i x -> i, fun () -> f x) l
        
    let pair f1 f2 =
      if int 1 = 0 then
        let v1 = f1 () in
        let v2 = f2 () in
        v1, v2
      else
        let v2 = f2 () in
        let v1 = f1 () in
        v1, v2

    let (|||) f1 f2 = one_of [f1;f2] ()
    let (&&&) = pair

  end

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


  type ('a, 'x) trace = (X.step * ('a, 'x) Conf.t, 'x) Trace.t

  let trace ?(random=Random.State.make_self_init ()) f l : _ trace =
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


  type ('a, 'x) tree = (X.step * ('a, 'x) Conf.t, 'x) Tree.t
  
  module MS = Multicont.Shallow

  let tree f l : _ tree =
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
                let copy = Conf.Backtrack.checkpoint conf in
                let k = Multicont.Shallow.promote k in
                Tree.Choice (List.init n (fun v ->
                    Conf.Backtrack.restore conf copy;
                    go k v))
              )
            | _ -> None
        }
    in
    go (MS.promote @@ S.fiber @@ Conf.State.run conf) f
  
end
