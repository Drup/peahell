open Ppxlib

open Ast_builder.Default

module VarSet = Set.Make(String)

let lun_pat ~loc p =
  let lbl = Located.mk ~loc "lun" in
  pexp_extension ~loc (lbl, PPat (p, None))

let eident ~loc s =
  pexp_ident ~loc @@ Located.mk ~loc @@ Longident.parse s
let tident ~loc s l =
  ptyp_constr ~loc (Located.mk ~loc @@ Longident.parse s) l

(** Return all the variables in a pattern. *)
let find_vars p =
  let o = object
    inherit [_] Ast_traverse.fold as super
    method! pattern_desc p acc = match p with
      | Ppat_var l -> VarSet.add l.txt acc
      | _ -> super#pattern_desc p acc
  end
  in VarSet.to_list @@ o#pattern p VarSet.empty

(** Erase all the variables except the one provided. *)
let erase_all_vars_except var p = object
  inherit Ast_traverse.map as super
  method! pattern_desc = function
    | Ppat_var v when v.txt <> var -> Ppat_any
    | p -> super#pattern_desc p
end#pattern p


let expander ~ctxt matchee cases =
  let loc =
    { (Expansion_context.Extension.extension_point_loc ctxt)
      with loc_ghost = true }
  in
  let new_matchee =
    let loc = matchee.pexp_loc in
    pexp_apply ~loc
      (eident ~loc "Peahell.Eval.I.view")
      [Nolabel,
       pexp_constraint ~loc 
         matchee
         (tident ~loc "Peahell.Eval.I.t" [ptyp_any ~loc])
      ]
  in
  let cases =
    let map_cases case =
      let pc_rhs =
        let vars = find_vars case.pc_lhs in
        let vbs =
          List.map (fun v ->
              let pat = pvar ~loc v in
              let lense_pat =
                erase_all_vars_except v case.pc_lhs
              in
              let expr =
                pexp_apply ~loc
                  (eident ~loc "Peahell.Eval.I.sub")
                  [ Nolabel, matchee;
                    Nolabel, lun_pat ~loc lense_pat ;
                    Nolabel, evar ~loc v ]
              in
              Ast_helper.Vb.mk
                ~loc
                ~attrs:[Merlin_helpers.hide_attribute]
                pat expr
            ) vars
        in
        if List.is_empty vbs then
          case.pc_rhs
        else
          pexp_let ~loc Nonrecursive vbs case.pc_rhs
      in
      {case with pc_rhs}
    in
    List.map map_cases cases
  in
  pexp_match ~loc new_matchee cases


let context = Extension.Context.Expression
let extracter () =
  Ast_pattern.(single_expr_payload @@ pexp_match __ __)

let ext =
  Extension.V3.declare "lensed"
    context
    (extracter())
    expander

let () =
  Driver.register_transformation
    ~extensions:[ext]
    "peahell"
