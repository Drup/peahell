open Peahell__.Eval

type name = string
[@@deriving show { with_path = false }]

type lbl = Tau | Send of name | Recv of name
and proc =
  | Nop
  | Choice of proc list
  | Nu of name * proc
  | Lbl of lbl * proc
  | Par of proc * proc
[@@deriving show { with_path = false }]

(** The type of our reduction and our step *)
type red = proc I.t -> unit
type step = lbl

exception Stuck of string
let stuck fmt = Fmt.kstr (fun s -> raise @@ Stuck s) fmt

module Interp = Make(struct type nonrec step = step end)

let compatible_lbl l1 l2 = match l1, l2 with
  | Send n, Recv n' | Recv n', Send n -> n = n'
  | _ -> false

let rec head p0 =
  match%lensed p0 with
  | Nop -> stuck "nop"
  | Lbl (l, p) ->
    I.view l, I.view p
  | Par (p1, p2) ->
    Interp.Choice.one_of [
      (fun () ->
         let l, p1 = head p1 in l, Par (p1, I.view p2)
      );
      (fun () ->
         let l, p2 = head p2 in l, Par (I.view p1, p2)
      );
      (fun () ->
         let l1, p1 = head p1 in
         let l2, p2 = head p2 in
         if compatible_lbl l1 l2 then
           Tau, Par (p1, p2)
         else
           stuck "%a vs. %a" pp_lbl l1 pp_lbl l2
      )
    ] ()
  | Choice p ->
    head @@ Interp.Choice.one_of @@ I.list p
  | Nu (n, p) ->
    let n = I.view n in
    let l, p' = head p in
    match l with
    | Send n' when n = n' -> stuck "Bound %a" pp_lbl l
    | Recv n' when n = n' -> stuck "Bound %a" pp_lbl l
    | _ -> l, Nu (n, p')

and eval p =
  let l, p' = head p in
  let p' = I.set p p' in
  Interp.step l;
  eval p'

(** Printing the trace *)

let pp_stateconf fmt (s, [I p] : step * (red, unit) Conf.t) =
  Fmt.pf fmt "-%a→ @[%a@]@," pp_lbl s pp_proc p
[@@warning "-8"]

let pp_tree = Fmt.vbox @@ Tree.pp pp_stateconf Fmt.nop

module P = struct

  let (||) p1 p2 = Par (p1, p2)
  let (+) p1 p2 = Choice [p1; p2]
  let ( ** ) l p = Lbl (l, p)

  let mu n p = Nu (n, p)
  let (let*) n f = Nu (n, f @@ Send n)
  let (~-) = function Tau -> Tau | Send n -> Recv n | Recv n -> Send n
  let nop = Nop
end
let e0 =
  P.(let* a = "a" in
     let* b = "b" in
     a ** nop + b ** nop
     || (Send "c" ** -a ** nop)
    )
  

let () =
  Fmt.pr "Running %a@." pp_proc e0;
  let tree = Interp.tree eval Arg.[i e0] in
  Fmt.pr "trace:@.%a@." pp_tree tree
  (* let v = E.run eval ~state e0 in *)
  (* Fmt.pr "v: %a@." pp_v trace *)
