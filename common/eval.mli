
(** A Thunked Sequence with a terminal element. *)
module Trace : sig
  type ('state, 'v) node =
      Cons of 'state * ('state, 'v) t
    | Return of 'v
    | Error of exn
  and ('state, 'v) t = unit -> ('state, 'v) node

  val as_seq :
    ('a, 'b) t -> [> `Error of exn | `Ret of 'b | `Step of 'a ] Seq.t

  val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
end

(** A Thunked Tree with a terminal element. *)
module Tree : sig
  type ('state, 'v) node =
      Cons of 'state * ('state, 'v) t
    | Return of 'v
    | Error of exn
    | Choice of ('state, 'v) t list
  and ('state, 'v) t = unit -> ('state, 'v) node
  val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
end

(** Lensed expression, to be explored Inductively *)
module I : sig
  
  type !'a t
  (** The handle to a lensed element. *)

  val view : 'a t -> 'a
  (** [view e] returns the current content. *)

  exception Invalid_subterm :
              'b t * ('b, 'b, 'a, 'a) Lun.t -> exn

  val sub :'a t -> ('a, 'a, 'b, 'b) Lun.t  -> 'b -> 'b t
  (** [sub e l v] indicates that the lensed element should now focus on
      the subposition [l].
      The expected value at this position is [v].

      The intended use for this function is to take subexpression
      during inductive walks, complemented by {!Accessor} lenses.

      For instance, the code [match (I.view e) with App (e1, _) -> ...]
      should be complemented by
      [let e1 = I.sub e Accessor.(app_lens @> fst) e1].
  *)

  val set : 'a t -> 'a -> 'a t
  (** [set e newcontent] changes the content of the currently lensed position. *)

  val map : ('a -> 'a) -> 'a t -> 'a t
  (** [map f e] is [set e (f (view e))], but more efficient. *)
    
  val list : 'a list t -> 'a t list
  (** [list es] is the list of all lensed position in [es]. *)
end
  
(** The arguments of an interpreter. *)  
module Arg : sig

  type _ one
  (** A single argument. *)

  val i : 'a -> ('a I.t) one
  (** [i v] is a lensed expression argument whose initial value is [v]. *)

  val m : snapshot:('a -> 'a) -> 'a -> 'a one
  (** [m ~snapshot v] is a mutable argument whose initial value is [v].
      [snapshot] is used to record the configuration in traces. *)

  val ref : 'a -> ('a ref) one
  (** [ref v] is a mutable argument specialized for {!Stdlib.ref}erences. *)

  val pure : 'a -> 'a one
  (** [pure v] is an immutable argument. *)

  type ('f,'r) t =
    | [] : ('a, 'a) t
    | (::) : ('a) one * ('b, 'c) t -> ('a -> 'b, 'c) t
    (** A list of arguments, to be build using list syntax.
        ['f] denotes the type of the interpretor, and ['r] its return type.
        For instance [ [ref 0; i e] ]
        has type [(int ref -> expr I.t -> 'a, 'a) Arg.t].
    *)
end

(** A snapshot of the configuration. To be used in traces. *)
module Conf : sig
  type (_,_) one =
    | I : 'a -> ('a I.t -> 'x, 'x) one
    | M : 'a -> ('a -> 'x, 'x) one

  type ('a, 'b) t =
    | [] : ('a, 'a) t
    | (::) : ('a, 'b) one * ('b, 'c) t -> ('a, 'c) t
end

(** Interpreter functions, parameterized by a notion of step. *)
module Make (X : sig type step end) : sig

  val step : X.step -> unit
  (** [step s] can be used to mark a step. *)

  val enter : unit -> unit
  (** [enter ()] can be used to mark the entering in a scope. *)

  (** Non deterministic choice. *)
  module Choice : sig
    val int : int -> int

    val one_of : 'a list -> 'a

    val tasks_assoc : ('a * (unit -> 'b)) list -> ('a * 'b) list

    val tasks : (unit -> 'a) list -> 'a list

    val map_nd : ('a -> 'b) -> 'a list -> 'b list

    val pair : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b

    val ( ||| ) : (unit -> 'a) -> (unit -> 'a) -> 'a
    val ( &&& ) : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
  end

  val run :
    ?random:Random.State.t -> 'a -> ('a, 'b) Arg.t -> 'b
  (** [run f args] run the interpretor [f] in a big-step fashion
      with the provided [args] and returns the final result.
      When a {!Choice} is presented, it pick randomly.
  *)

  type ('a, 'x) trace = (X.step * ('a, 'x) Conf.t, 'x) Trace.t

  val trace :
    ?random:Random.State.t -> 'a -> ('a, 'b) Arg.t ->  ('a, 'b) trace
  (** [run f args] run the interpretor [f] step by step
      with the provided [args] and returns a trace.
      When a {!Choice} is presented, it pick randomly.
  *)

  type ('a, 'x) tree = (X.step * ('a, 'x) Conf.t, 'x) Tree.t

  val tree : 'a -> ('a, 'b) Arg.t -> ('a, 'b) tree
  (** [run f args] run the interpretor [f] step by step
      with the provided [args] and returns a tree.
      When a {!Choice} is presented, it explore all branches.
  *)
end
