open Peahell__.Eval

type v = Int of int | Lam of (expr -> expr)
and expr =
  | Value of v
  | App of expr * expr
  | Add of expr list
  | Get
  | Set of expr
[@@deriving show { with_path = false }]

type context = 
  | CHole
  | CAppL of context * expr
  | CAppR of v * context
  | CAdd of context_list
  | CSet of context
and context_list = v list * context * expr list

let v x = Value x

let rec plug c e0 = match c with
  | CHole -> e0
  | CAppL (c, e) -> App (plug c e0, e)
  | CAppR (x, c) -> App (v x, plug c e0)
  | CAdd (pre, c, post) ->
    Add (List.map v pre @ plug c e0 :: post)
  | CSet c -> Set (plug c e0)

let map_context f l =
  let rec aux f acc = function
    | [] -> []
    | h :: t ->
      let ctxs : context_list = (List.rev acc, CHole, t) in
      let h' = f ctxs h in 
      h' :: aux f (h' :: acc) t
  in
  aux f [] l

module E = Expr(struct
    type nonrec t = expr
    type nonrec context = context
    let plug = plug
    let pp = pp_expr
  end)
open E.Infix

module Interp = Make(struct
    type state = v
    type configuration = E.t
    type value = v
  end)

let pp_stateconf fmt (st, c) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v st E.pp c
let pp_ret fmt (st, v) =
  Fmt.pf fmt "@[%a@] × @[%a@]" pp_v st pp_v v

let pp_trace =
  Fmt.vbox @@ Trace.pp ~pp_sep:(Fmt.any " →@ ") pp_stateconf pp_ret

let sum vs =
  Int (List.fold_left (fun s v ->
      match v with Int i -> s+i | _ -> failwith "not an int"
    ) 0 vs)

let rec eval e0 =
  let eval = Interp.recurse eval in
  match E.view e0 with
  | Value v -> v
  | App (f, arg) ->
    let f' = eval (e0 $>> CAppL (CHole, arg) ^> f) in
    let arg' = eval (e0 $>> CAppR (f', CHole) ^> arg) in
    begin match f' with
      | Lam l -> eval (e0 $> l (v arg'))
      | _ -> failwith "foo"
    end
  | Add l ->
    let vs =
      map_context (fun ctxs_i e_i -> eval (e0 $>> CAdd ctxs_i ^> e_i)) l
    in
    sum vs
  | Get -> Interp.get ()
  | Set e ->
    let v = eval (e0 $>> CSet CHole ^> e) in
    Interp.set v;
    v
    

let e0 =
  App (v @@ Lam (fun x -> Add [x; Get; v (Int 2)]), Set (v (Int 3)))

let c0 =
  CHole ^> e0


let () =
  Fmt.pr "Running %a@." E.pp c0;
  let state = Int 0 in
  let trace = Interp.steps eval ~state c0 in
  Fmt.pr "trace:@.%a@." pp_trace trace
