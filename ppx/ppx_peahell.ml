open Ppxlib

open Ast_builder.Default

module VMap = Map.Make(String)

let lun_pat ~loc p =
  let lbl = Located.mk ~loc "lun" in
  pexp_extension ~loc (lbl, PPat (p, None))

let elident ~loc s =
  pexp_ident ~loc @@ Located.mk ~loc @@ Longident.parse s
let tlident ~loc s l =
  ptyp_constr ~loc (Located.mk ~loc @@ Longident.parse s) l

(** Return all the variables in a pattern. *)
let find_vars p =
  let o = object
    inherit [_] Ast_traverse.fold as super
    method! pattern_desc p acc = match p with
      | Ppat_var v | Ppat_alias (_, v) -> VMap.add v.txt v acc
      | _ -> super#pattern_desc p acc
  end
  in List.map snd @@ VMap.bindings @@ o#pattern p VMap.empty

(** Erase all the variables except the one provided. *)
let erase_vars var p = object
  inherit Ast_traverse.map as super
  method! pattern_desc desc = match desc with
    | Ppat_var v | Ppat_alias (_, v) when v.txt <> var ->
      Ppat_any
    | _ ->
      super#pattern_desc desc
end#pattern p

(** Simplify a pattern to avoid talking about anything except bound variables.
*)
let simplify_pattern p =
  let o = object
    inherit [bool] Ast_traverse.fold_map as super
    method! pattern_desc desc has_var = match desc with
      | Ppat_var _ as desc ->
        desc, true
      | Ppat_alias (_, v) ->
        Ppat_var v, true
      | p ->
        let p', has_var' = super#pattern_desc p false in
        if has_var' then
          p', true
        else
          Ppat_any, has_var
  end
  in fst @@ o#pattern p false

let expander ~ctxt matchee cases =
  let loc =
    { (Expansion_context.Extension.extension_point_loc ctxt)
      with loc_ghost = true }
  in
  let new_matchee =
    let loc = matchee.pexp_loc in
    pexp_apply ~loc
      (elident ~loc "Peahell.Eval.I.view")
      [Nolabel,
       pexp_constraint ~loc 
         matchee
         (tlident ~loc "Peahell.Eval.I.t" [ptyp_any ~loc])
      ]
  in
  let cases =
    let map_cases case =
      let pc_rhs =
        let vars = find_vars case.pc_lhs in
        let vbs =
          List.map (fun v ->
              let pat =
                Merlin_helpers.focus_pattern @@
                ppat_var ~loc:v.loc v
              in
              let lense_pat =
                simplify_pattern @@
                erase_vars v.txt case.pc_lhs
              in
              let expr =
                pexp_apply ~loc
                  (elident ~loc "Peahell.Eval.I.sub")
                  [ Nolabel, matchee;
                    Nolabel, lun_pat ~loc lense_pat ;
                    Nolabel, pexp_ident ~loc:v.loc @@ Located.map_lident v ]
              in
              Ast_helper.Vb.mk
                ~loc
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
