open Peahell__.Eval

type v = Int of int | Lam of (expr -> expr)
and expr =
  | V of v
  | App of expr * expr
  | Add of expr list
  | Get
  | Set of expr
[@@deriving show { with_path = false }]

module LensV = struct

  let int =
    let get = function Int v -> v | _ -> invalid_arg "Wrong Constructor"
    and set e' e = match e with Int _ -> Int e' | _ -> invalid_arg "Wrong Constructor"
    in {Lens. get; set}
  
  let lam =
    let get = function Lam v -> v | _ -> invalid_arg "Wrong Constructor"
    and set e' e = match e with Lam _ -> Lam e' | _ -> invalid_arg "Wrong Constructor"
    in {Lens. get; set}
  
end

module LensExpr = struct

  let value =
    let get = function V v -> v | _ -> invalid_arg "Wrong Constructor Value"
    and set e' e = match e with V _ -> V e' | _ -> invalid_arg "Wrong Constructor Value"
    in {Lens. get; set}

  let appL =
    let get = function App (v,_) -> v | _ -> invalid_arg "Wrong Constructor App"
    and set e' e = match e with App (_, v) -> App (e', v) | _ -> invalid_arg "Wrong Constructor App"
    in {Lens. get; set}

  let appR =
    let get = function App (_,v) -> v | _ -> invalid_arg "Wrong Constructor App"
    and set e' e = match e with App (v, _) -> App (v, e') | _ -> invalid_arg "Wrong Constructor App"
    in {Lens. get; set}

  let add =
    let get = function Add v -> v | _ -> invalid_arg "Wrong Constructor Add"
    and set e' e = match e with Add _ -> Add e' | _ -> invalid_arg "Wrong Constructor Add"
    in {Lens. get; set}

  let set =
    let get = function Set v -> v | _ -> invalid_arg "Wrong Constructor Set"
    and set e' e = match e with Set _ -> Set e' | _ -> invalid_arg "Wrong Constructor Set"
    in {Lens. get; set}

end

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
  match I.view e0 with
  | V v -> v
  | App (f, arg) ->
    let f = I.sub e0 LensExpr.appL f in
    let arg = I.sub e0 LensExpr.appR arg in
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
    let l = I.sub e0 LensExpr.add l in
    let l' = I.list l in
    let vs = List.map (eval st) l' in
    let v = sum vs in
    stepV "add" e0 ~as_:v
  | Get ->
    let v = !st in
    stepV "get" e0 ~as_:v
  | Set e ->
    let e = I.sub e0 LensExpr.set e in
    let v = eval st e in
    st := v;
    stepV "set" e0 ~as_:v

(** Printing the trace *)

let pp_stateconf fmt (s, [M st; I e] : step * (red, v) Conf.t) =
  Fmt.pf fmt "-%s→ @[%a@] × @[%a@]@," s pp_v !st pp_expr e
[@@warning "-8"]

let pp_trace = Fmt.vbox @@ Trace.pp pp_stateconf (Fmt.box pp_v)


let e0 =
  App (value @@ Lam (fun x -> Add [x; Get; value (Int 2)]), Set (value (Int 3)))
  (* Add [value @@ Int 2; Set (value @@ Int 3); Get] *)

let () =
  Fmt.pr "Running %a@." pp_expr e0;
  let state = Int 0 in
  let trace = Interp.trace eval Arg.[ref state; i e0] in
  Fmt.pr "trace:@.%a@." pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
  
