open Peahell__.Eval

type name = string
and lbl = Tau | Send of name | Recv of name
and proc =
  | Nop
  | Choice of proc list
  | Mu of name * proc
  | Lbl of lbl * proc
  | Par of proc * proc
[@@deriving show { with_path = false }]

module LensProc = struct

  let parL =
    let get = function Par (v,_) -> v | _ -> invalid_arg "Wrong Constructor Par"
    and set e' e = match e with Par (_, v) -> Par (e', v) | _ -> invalid_arg "Wrong Constructor Par"
    in {Lens. get; set}

  let parR =
    let get = function Par (_,v) -> v | _ -> invalid_arg "Wrong Constructor Par"
    and set e' e = match e with Par (v, _) -> Par (v, e') | _ -> invalid_arg "Wrong Constructor Par"
    in {Lens. get; set}

  let choice =
    let get = function Choice v -> v | _ -> invalid_arg "Wrong Constructor Choice"
    and set e' e = match e with Choice _ -> Choice e' | _ -> invalid_arg "Wrong Constructor Choice"
    in {Lens. get; set}

  let muL =
    let get = function Mu (v,_) -> v | _ -> invalid_arg "Wrong Constructor Mu"
    and set e' e = match e with Mu (_, v) -> Mu (e', v) | _ -> invalid_arg "Wrong Constructor Mu"
    in {Lens. get; set}

  let muR =
    let get = function Mu (_,v) -> v | _ -> invalid_arg "Wrong Constructor Mu"
    and set e' e = match e with Mu (v, _) -> Mu (v, e') | _ -> invalid_arg "Wrong Constructor Mu"
    in {Lens. get; set}

end

(** The type of our reduction and our step *)
type red = proc I.t -> unit
type step = lbl

exception Stuck

module Interp = Make(struct type nonrec step = step end)

let compatible_lbl l1 l2 = match l1, l2 with
  | Send n, Recv n' | Recv n', Send n -> n = n'
  | _ -> false

let rec try_eval p0 =
  match I.view p0 with
  | Nop -> raise Stuck 
  | Lbl (l, p) -> l, p
  | Par (p1, p2) ->
    let p1 = I.sub p0 LensProc.parL p1 in
    let p2 = I.sub p0 LensProc.parR p2 in
    Interp.Choice.one_of [
      (fun () ->
         let l, p1 = try_eval p1 in l, Par (p1, I.view p2)
      );
      (fun () ->
         let l, p2 = try_eval p2 in l, Par (I.view p1, p2)
      );
      (fun () ->
         let l1, p1 = try_eval p1 in
         let l2, p2 = try_eval p2 in
         if compatible_lbl l1 l2 then
           Tau, Par (p1, p2)
         else
           raise Stuck
      )
    ] ()
  | Choice p ->
    let p = I.sub p0 LensProc.choice p in
    try_eval @@ Interp.Choice.one_of @@ I.list p
  | Mu (n, p) ->
    let p = I.sub p0 LensProc.muR p in
    let l, p' = try_eval p in
    match l with
    | Send n' when n = n' -> raise Stuck
    | Recv n' when n = n' -> raise Stuck
    | _ -> l, Mu (n, p')

and eval p =
  let l, p' = try_eval p in
  let p' = I.set p p' in
  Interp.step l;
  eval p'

(** Printing the trace *)

let pp_stateconf fmt (s, [I p] : step * (red, unit) Conf.t) =
  Fmt.pf fmt "-%a→ @[%a@]@," pp_lbl s pp_proc p
[@@warning "-8"]

let pp_trace = Fmt.vbox @@ Trace.pp pp_stateconf Fmt.nop

module P = struct

  let (||) p1 p2 = Par (p1, p2)
  let (+) p1 p2 = Choice [p1; p2]
  let ( ** ) l p = Lbl (l, p)

  let mu n p = Mu (n, p)
  let (let*) n f = Mu (n, f @@ Send n)
  let (~-) = function Tau -> Tau | Send n -> Recv n | Recv n -> Send n
  let nop = Nop
end
let e0 =
  P.(let* a = "a" in
     let* b = "b" in
     (a ** nop + b ** nop)
     || (Send "c" ** -a ** nop)
    )
  

let () =
  Fmt.pr "Running %a@." pp_proc e0;
  let trace = Interp.trace eval Arg.[i e0] in
  Fmt.pr "trace:@.%a@." pp_trace trace
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
