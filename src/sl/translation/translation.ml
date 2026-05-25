open SLsyntax
open Typecheck
open Common

module Varset = Syntax__Varset

let rec gen_top_level_types (tls : top_level list) =
  match tls with
    | tl::rest -> 
    (match tl with
      | TLEffSig (name, effect_sigs) -> 
        effect_sigs_context := !effect_sigs_context@[(name, effect_sigs)];
        effect_names := !effect_names@[name];
        gen_top_level_types rest

      | TLEffZSig (name, effect_sigs) -> 
        effect_sigs_context := !effect_sigs_context@[(name, effect_sigs)];
        effectz_names := !effectz_names@[name];
        gen_top_level_types rest

      | TLPolyAbs (name, type_params, cap_params, label_params, params, return_cty, _) ->
        let params_ty = List.map (fun (x, ty) -> (Some x, ty)) params in
        let fun_ty = TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; region = RTop; return_cty } in
        let ty = List.fold_right (fun (tv, k) ty -> TForall (tv, k, [], ty)) type_params fun_ty in
        (name, ty)::gen_top_level_types rest

      | TLAbs (name, cap_params, label_params, params, return_cty, _) ->
        let params_ty = List.map (fun (x, ty) -> (Some x, ty)) params in
        (name, TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; region = RTop; return_cty })::gen_top_level_types rest

      | TLType type_defs ->
        check_type_defs type_defs;
        let type_defs_assoc = List.map (fun ({ type_name; type_params; type_cons }: SLsyntax.typedef) -> (type_name, (type_params, type_cons))) type_defs in
        type_defs_context := !type_defs_context@type_defs_assoc;
        gen_top_level_types rest

      | _ -> gen_top_level_types rest
    )
    | _ -> []
  
let remove_assoc key bindings =
  List.filter (fun (key', _) -> key <> key') bindings

let infer_top_level_type top_level_fun_types
    name type_params cap_params label_params params return_cty body =
  let fun_expr =
    SLsyntax.Fun {
      captured_set = None, Varset.empty;
      cap_params;
      label_params;
      params;
      return_cty;
      body
    }
  in
  let rctx = { empty_region_ctx with kind_env = type_params } in
  let typed_fun = type_expr rctx (Labels []) [] [] top_level_fun_types fun_expr in
  let closed_ty, residual =
    close_forall_constraints type_params (ty_of_cty typed_fun.expr_cty)
      typed_fun.region_constraints
  in
  if residual <> [] then
    typing_error
      "Top-level function %s has uncaptured subregion constraints"
      name;
  closed_ty

let enrich_top_level_types tls top_level_fun_types =
  List.fold_left
    (fun env tl ->
      match tl with
      | TLPolyAbs (name, type_params, cap_params, label_params, params, return_cty, body) ->
        let ty =
          infer_top_level_type env
            name type_params cap_params label_params params return_cty body
        in
        (name, ty) :: remove_assoc name env
      | TLAbs (name, cap_params, label_params, params, return_cty, body) ->
        let ty =
          infer_top_level_type env
            name [] cap_params label_params params return_cty body
        in
        (name, ty) :: remove_assoc name env
      | _ -> env)
    top_level_fun_types tls

let typecheck_toplevels (tls : top_level list) : unit =
  let top_level_fun_types = gen_top_level_types tls in
  let top_level_fun_types = enrich_top_level_types tls top_level_fun_types in
  List.iter (fun tl ->
      match tl with
      | TLPolyAbs (_, type_params, cap_params, label_params, params, return_cty, body) ->
        let fun_expr = SLsyntax.Fun { captured_set = None, Varset.empty; cap_params; label_params; params; return_cty; body } in
        let rctx = { empty_region_ctx with kind_env = type_params } in
        let typed_fun = type_expr rctx (Labels []) [] [] top_level_fun_types fun_expr in
        let _, residual =
          close_forall_constraints type_params (ty_of_cty typed_fun.expr_cty)
            typed_fun.region_constraints
        in
        if residual <> [] then
          typing_error
            "Top-level polymorphic function has uncaptured subregion constraints"
      | TLAbs (_, cap_params, label_params, params, return_cty, body) ->
        let fun_expr = SLsyntax.Fun { captured_set = None, Varset.empty; cap_params; label_params; params; return_cty; body } in
        let typed_fun = type_expr empty_region_ctx (Labels []) [] [] top_level_fun_types fun_expr in
        if typed_fun.region_constraints <> [] then
          typing_error
            "Top-level function has uncaptured subregion constraints"
      | _ -> ()
    )
    tls
