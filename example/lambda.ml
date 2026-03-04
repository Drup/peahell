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

let pp_stateconf fmt ([st; e] : (_, v conf * v) E.Arg.list) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v st pp_expr e
let pp_ret fmt (st, v) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v (Conf.view st) pp_v v

let pp_trace =
  Fmt.vbox @@ Trace.pp ~pp_sep:(Fmt.any " →@.") (Fmt.pair Fmt.nop pp_stateconf) pp_ret

let sum vs =
  Int (List.fold_left (fun s v ->
      match v with Int i -> s+i | _ -> failwith "not an int"
    ) 0 vs)

let stepV e0 ~as_:x =
  let _ = Conf.set e0 @@ value x in
  E.step ();
  x

let rec eval st e0 =
  match Conf.view e0 with
  | Value v -> st, v
  | App (f, arg) ->
    let f = Conf.sub e0 LensExpr.appL f in
    let arg = Conf.sub e0 LensExpr.appR arg in
    let st', f' = eval st f in
    let st'', arg' = eval st' arg in
    begin match f' with
      | Lam l ->
        let e' = Conf.set e0 @@ l (value arg') in
        step ();
        eval st'' e'
      | _ -> failwith "Not a lambda"
    end
  | Add l ->
    let l = Conf.sub e0 LensExpr.add l in
    let l' = Conf.list l in
    let st', vs = List.fold_left_map eval st l' in
    let v = sum vs in
    st', stepV e0 ~as_:v
  | Get ->
    let v = Conf.view st in
    st, stepV e0 ~as_:v
  | Set e ->
    let e = Conf.sub e0 LensExpr.set e in
    let st', v = eval st e in
    let st'' = Conf.set st' v in
    st'', stepV e0 ~as_:v
    

let e0 =
  App (value @@ Lam (fun x -> Add [x; Get; value (Int 2)]), Set (value (Int 3)))
  (* Add [v @@ Int 2; Set (v @@ Int 3); Get] *)

let () =
  Fmt.pr "Running %a@." pp_expr e0;
  let state = Int 0 in
  let trace = E.steps eval [state; e0] in
  Fmt.pr "trace:@.%a →@.%a@." pp_stateconf [state; e0] pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
  
