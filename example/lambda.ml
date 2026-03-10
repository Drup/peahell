open Peahell.Eval

type v = Int of int | Lam of (expr -> expr)
and expr =
  | V of v
  | App of expr * expr
  | Add of expr list
  | Get
  | Set of expr
[@@deriving show { with_path = false }, lun]

let value x = V x

(** The type of our reduction and our step *)
type red = v ref -> expr I.t -> v
type step = string

module Interp = Make(struct type nonrec step = step end)

let sum vs =
  Int (List.fold_left (fun s v ->
      match v with Int i -> s+i | _ -> failwith "not an int"
    ) 0 vs)

let stepV s e0 ~as_:x =
  let _ = I.set e0 @@ value x in
  Interp.step s;
  x

let rec eval st e0 =
  match%lensed e0 with
  | V v -> I.view v
  | App (f, arg) ->
    let f' = eval st f in
    let arg' = eval st arg in
    begin match f' with
      | Lam l ->
        let e' = I.set e0 @@ l (value arg') in
        Interp.step "app";
        eval st e'
      | _ -> failwith "Not a lambda"
    end
  | Add l ->
    let l' = I.list l in
    let vs = List.map (eval st) l' in
    let v = sum vs in
    stepV "add" e0 ~as_:v
  | Get ->
    let v = !st in
    stepV "get" e0 ~as_:v
  | Set e ->
    let v = eval st e in
    st := v;
    stepV "set" e0 ~as_:v

(** Printing the trace *)

let pp_stateconf fmt (s, [M st; I e] : step * (red, v) Conf.t) =
  Fmt.pf fmt "-%s→ @[%a@] × @[%a@]@," s pp_v !st pp_expr e
[@@warning "-8"]

let pp_trace = Fmt.vbox @@ Trace.pp pp_stateconf (Fmt.box pp_v)

module L = struct
  let (@@) f arg = App (f,arg)
  let (+) x y = Add [x;y]
  let add l = Add l
  let lam f = value (Lam f)
  let i n = value (Int n)
end

let e0 =
  L.( lam (fun x -> x + Get + i 2) @@ Set (i 3))

let () =
  Fmt.pr "Running %a@." pp_expr e0;
  let state = Int 0 in
  let trace = Interp.trace eval Arg.[ref state; i e0] in
  Fmt.pr "trace:@.%a@." pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
  
