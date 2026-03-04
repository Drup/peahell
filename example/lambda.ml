open Peahell__.Eval

type v = Int of int | Lam of (expr -> expr)
and expr =
  | Value of v
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
    let get = function Value v -> v | _ -> invalid_arg "Wrong Constructor Value"
    and set e' e = match e with Value _ -> Value e' | _ -> invalid_arg "Wrong Constructor Value"
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

let value x = Value x

module E = Make(struct
    type step = unit
  end)
open E

type conf = (v ref -> expr I.t -> v, v) Arg.list

let pp_stateconf fmt ( [M st; I e] : conf) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v !(st.v) pp_expr e
[@@warning "-8"]

let pp_ret fmt v =
  Fmt.pf fmt "@[%a@]" pp_v v

let pp_trace =
  Fmt.vbox @@ Trace.pp ~pp_sep:(Fmt.any " →@.") (Fmt.pair Fmt.nop pp_stateconf) pp_ret

let sum vs =
  Int (List.fold_left (fun s v ->
      match v with Int i -> s+i | _ -> failwith "not an int"
    ) 0 vs)

let stepV e0 ~as_:x =
  let _ = I.set e0 @@ value x in
  E.step ();
  x

let rec eval st e0 =
  match I.view e0 with
  | Value v -> v
  | App (f, arg) ->
    let f = I.sub e0 LensExpr.appL f in
    let arg = I.sub e0 LensExpr.appR arg in
    let f' = eval st f in
    let arg' = eval st arg in
    begin match f' with
      | Lam l ->
        let e' = I.set e0 @@ l (value arg') in
        step ();
        eval st e'
      | _ -> failwith "Not a lambda"
    end
  | Add l ->
    let l = I.sub e0 LensExpr.add l in
    let l' = I.list l in
    let vs = List.map (eval st) l' in
    let v = sum vs in
    stepV e0 ~as_:v
  | Get ->
    let v = !st in
    stepV e0 ~as_:v
  | Set e ->
    let e = I.sub e0 LensExpr.set e in
    let v = eval st e in
    st := v;
    stepV e0 ~as_:v
    

let e0 =
  (* App (value @@ Lam (fun x -> Add [x; Get; value (Int 2)]), Set (value (Int 3))) *)
  Add [value @@ Int 2; Set (value @@ Int 3); Get]

let () =
  Fmt.pr "Running %a@." pp_expr e0;
  let state = Int 0 in
  let trace = E.steps eval [M (M.ref state); I e0] in
  Fmt.pr "trace:@.%a@." pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
  
