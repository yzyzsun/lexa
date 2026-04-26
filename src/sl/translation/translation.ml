open SLsyntax
open Typecheck

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
        let params_ty = List.map snd params in
        let fun_ty = TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; return_cty } in
        let ty = List.fold_right (fun (tv, k) ty -> TForall (tv, k, ty)) type_params fun_ty in
        (name, ty)::gen_top_level_types rest

      | TLAbs (name, cap_params, label_params, params, return_cty, _) ->
        let params_ty = List.map snd params in
        (name, TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; return_cty })::gen_top_level_types rest

      | TLType type_defs ->
        check_type_defs type_defs;
        let type_defs_assoc = List.map (fun ({ type_name; type_params; type_cons }: SLsyntax.typedef) -> (type_name, (type_params, type_cons))) type_defs in
        type_defs_context := !type_defs_context@type_defs_assoc;
        gen_top_level_types rest

      | _ -> gen_top_level_types rest
    )
    | _ -> []
  

let typecheck_toplevels (tls : top_level list) : unit =
  let top_level_fun_types = gen_top_level_types tls in
  List.iter (fun tl ->
      match tl with
      | TLPolyAbs (_, type_params, cap_params, label_params, params, return_cty, body) ->
        let fun_expr = SLsyntax.Fun { captured_set = None, Varset.empty; cap_params; label_params; params; return_cty; body } in
        let rctx = { empty_region_ctx with kind_env = type_params } in
        ignore (type_expr rctx (Labels []) [] [] top_level_fun_types fun_expr)
      | TLAbs (_, cap_params, label_params, params, return_cty, body) ->
        let fun_expr = SLsyntax.Fun { captured_set = None, Varset.empty; cap_params; label_params; params; return_cty; body } in
        ignore (type_expr empty_region_ctx (Labels []) [] [] top_level_fun_types fun_expr)
      | _ -> ()
    )
    tls
