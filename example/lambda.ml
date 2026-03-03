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

let v x = Value x

module E = Make(struct
    type state = v
    type configuration = expr
    type step = unit
  end)

let pp_stateconf fmt (st, e, ()) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v st pp_expr e
let pp_ret fmt (st, v) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v st pp_v v

let pp_trace =
  Fmt.vbox @@ Trace.pp ~pp_sep:(Fmt.any " →@.") pp_stateconf pp_ret

let sum vs =
  Int (List.fold_left (fun s v ->
      match v with Int i -> s+i | _ -> failwith "not an int"
    ) 0 vs)

let return e ~as_:x =
  E.swap {view = (v x); lens = e.E.lens};
  E.step ();
  x

let rec eval e0 =
  match E.view e0 with
  | Value v -> v
  | App (f, arg) ->
    let f = E.sub e0 LensExpr.appL f in
    let arg = E.sub e0 LensExpr.appR arg in
    let f' = eval f in
    let arg' = eval arg in
    begin match f' with
      | Lam l ->
        let e' = E.map (fun _ -> l (v arg')) e0 in
        eval e'
      | _ -> failwith "foo"
    end
  | Add l ->
    let l = E.sub e0 LensExpr.add l in
    let vs = E.List.map eval l in
    let v = sum vs in
    return e0 ~as_:v    
  | Get ->
    let v = E.get () in
    return e0 ~as_:v
  | Set e ->
    let e = E.sub e0 LensExpr.set e in
    let v = eval e in
    E.set v;
    return e0 ~as_:v
    

let e0 =
  App (v @@ Lam (fun x -> Add [x; Get; v (Int 2)]), Set (v (Int 3)))
  (* Add [v @@ Int 2; Set (v @@ Int 3); Get] *)

let () =
  Fmt.pr "Running %a@." pp_expr e0;
  let state = Int 0 in
  let trace = E.steps eval ~state e0 in
  Fmt.pr "trace:@.%a →@.%a@." pp_stateconf (state, e0, ()) pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
  
