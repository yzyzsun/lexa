open Syntax
open SLsyntax
open Typed_ast
open Typecheck
open Common
open Syntax__Common

let get_effect_index effect_name =
  match List.find_index (fun (eff, _) -> eff = effect_name) !effect_sigs_context with
  | Some i -> i
  | None -> typing_error "Unknown effect name %s\n" effect_name

let find_label_index label captured_vars label_vars =
  match List.find_index (fun (l, _) -> l = label) label_vars with
  | None -> 
    (
      match captured_vars with
      | Capability _ -> typing_error "Couldn't find label %s in context\n" label
      | Labels labels -> (
          match List.find_opt (fun (l, _) -> l = label) labels with
          | None -> typing_error "Couldn't find label %s in context\n" label
          | Some _ -> LabelInfty
        )
    )
  | Some i -> LabelIndex i

let find_cap_index cap captured_vars cap_vars =
  match List.find_index (fun c -> c = cap) cap_vars with
  | None -> 
    (
      match captured_vars with
      | Capability c -> if c = cap then CapInfty else typing_error "Couldn't find capability variable %s in context\n" cap
      | Labels _ -> typing_error "Couldn't find capability variable %s in context 2\n" cap
    )
  | Some i -> CapIndex i

let translate_capability (cap: SLsyntax.capability) captured_vars cap_vars label_vars: capability =
  let (cap_var, labels) = cap in
  let cap_var' = (
    match cap_var with
    | Some cap -> Some (find_cap_index cap captured_vars cap_vars)
    | None -> None
  ) in
  let labels' = Varset.fold (fun label ls -> ((find_label_index label captured_vars label_vars), get_effect_index (find_effect_name label captured_vars label_vars))::ls) labels [] in
  (cap_var', labels')

let rec translate_SL (te: typed_expr): Syntax.expr =
  let type_of te = 
    let { expr_ty; _ } = te in expr_ty in

  let translate_hdl (hdl: Typed_ast.hdl): Syntax.hdl = 
    let { op_anno; op_name; op_params; op_body } = hdl in
    { op_anno; op_name; op_params; op_body = translate_SL op_body }
  in

  let translate_fundef (fundef: Typed_ast.fundef): Syntax.fundef =
    let { name; params; body; _ } = fundef in
    { name; params = List.map fst params; body = translate_SL body }
  in
  
  let { expr_desc = e; captured_vars; label_vars; cap_vars; _ } = te in
  match e with
  | Unit -> Int 0
  | Int i -> Int i
  | Float fl -> Float fl
  | Bool b -> Bool b
  | Str s -> Str s
  | Char c -> Char c
  | Arith (e1, op, e2) -> 
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    Arith (e1', op, e2')
  | Cmp (e1, op, e2) ->
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    Cmp (e1', op, e2')
  | Neg e -> 
    let e' = translate_SL e in
    Neg e'
  | BArith (e1, op, e2) ->
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    BArith (e1', op, e2')
  | New el -> 
    let el' = List.map translate_SL el in
    New el'
  | Get (e, i) ->
    let e' = translate_SL e in
    let i' = translate_SL i in
    Get (e', i')
  | Set (e, i, nv) ->
    let e' = translate_SL e in
    let i' = translate_SL i in
    let nv' = translate_SL nv in
    Set (e', i', nv')
  | Var x -> Var x
  | Stmt (e1, e2) ->
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    Stmt (e1', e2')
  | Let (x, e1, e2) ->
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    Let (x, e1', e2')
  | If (cond, e1, e2) ->
    let cond' = translate_SL cond in
    let e1' = translate_SL e1 in
    let e2' = translate_SL e2 in
    If (cond', e1', e2')
  | Prim f ->  Prim f
  | Fun { params; body;_ } ->
    let body' = translate_SL body in
    let params' = List.map fst params in
    Fun (params', body')
  | App { func; cap_insts; label_args; args } -> 
    let func' = translate_SL func in
    let fun_ty = type_of func in
    let args' = List.map translate_SL args in
    let metadata = (match fun_ty with
      | TFun { captured_set; _ } ->
        let captured_set' = translate_capability captured_set captured_vars cap_vars label_vars in
        let cap_insts' = List.map (fun inst -> translate_capability inst captured_vars cap_vars label_vars) cap_insts in
        let label_args' = List.map (fun arg -> find_label_index arg captured_vars label_vars) label_args in
        (captured_set', cap_insts', label_args')
      | _ -> ((None, []), [], [])
    ) in
    App (func', args', metadata)

  | Handle { captured_set; handle_body; handler_label = _; sig_name; handler_defs } ->
    let handle_body' = translate_SL handle_body in
    let handler_defs' = List.map translate_hdl handler_defs in
    let captured_set' = translate_capability captured_set captured_vars cap_vars label_vars in
    HandleZ { 
      handle_body = handle_body';
      sig_name;
      handler_defs = handler_defs';
      captured_set = captured_set';
    }

  | Raise { raise_label; raise_op; raise_args } ->
    let raise_args' = List.map translate_SL raise_args in
    let clue = find_label_index raise_label captured_vars label_vars in
    let clue_type, clue_label = (
      match clue with
      | LabelIndex i -> 0, i
      | LabelInfty -> 2, 0
    ) in
    RaiseZ { 
      clue_sig = find_effect_name raise_label captured_vars label_vars;
      clue_type;
      clue_label;
      raisez_op = raise_op;
      raisez_args = raise_args';
    }

  | Resume (cont, arg) ->
    let cont' = translate_SL cont in
    let arg' = translate_SL arg in
    let cont_ty = type_of cont in
    let captured_set' = (match cont_ty with
      | TCont { captured_set; _ } ->
        translate_capability captured_set captured_vars cap_vars label_vars
      | _ -> (None, [])
    ) in
    Resume (cont', arg', captured_set')
  
  | ResumeFinal (cont, arg) ->
    let cont' = translate_SL cont in
    let arg' = translate_SL arg in
    let cont_ty = type_of cont in
    let captured_set' = (match cont_ty with
      | TCont { captured_set; _ } ->
        translate_capability captured_set captured_vars cap_vars label_vars
      | _ -> (None, [])
    ) in
    ResumeFinal (cont', arg', captured_set')

  | Recdef (fundefs, e) -> 
    let fundefs' = List.map translate_fundef fundefs in
    let e' = translate_SL e in
    Recdef (fundefs', e')

  | Typecon (t, _, args) ->
    let args' = List.map translate_SL args in
    Typecon (t, args')

  | Match { match_expr; pattern_matching } ->
    let match_expr' = translate_SL match_expr in
    let pattern_matching' = List.map (fun match_clause ->
      let (pt, res) = match_clause in
      let res' = translate_SL res in
      (pt, res')
    ) pattern_matching in
    Match { match_expr = match_expr'; pattern_matching = pattern_matching' }
  | TypeApp (_, e) ->
    translate_SL e

let rec gen_top_level_types (tls : top_level list) =
  match tls with
    | tl::rest -> 
    (match tl with
      | TLEffZSig (name, effect_sigs) -> 
        effect_sigs_context := !effect_sigs_context@[(name, effect_sigs)];
        gen_top_level_types rest

      | TLPolyAbs (name, type_params, cap_params, label_params, params, return_ty, _) ->
        let params_ty = List.map snd params in
        let fun_ty = TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; return_ty } in
        let ty = List.fold_right (fun tv ty -> TForall (tv, ty)) type_params fun_ty in
        (name, ty)::gen_top_level_types rest

      | TLAbs (name, cap_params, label_params, params, return_ty, _) ->
        let params_ty = List.map snd params in
        (name, TFun { captured_set=None, Varset.empty; cap_params; label_params; params_ty; return_ty })::gen_top_level_types rest

      | TLType type_defs ->
        check_type_defs type_defs;
        let type_defs_assoc = List.map (fun ({ type_name; type_params; type_cons }: SLsyntax.typedef) -> (type_name, (type_params, type_cons))) type_defs in
        type_defs_context := !type_defs_context@type_defs_assoc;
        gen_top_level_types rest

      | _ -> gen_top_level_types rest
    )
    | _ -> []
  

let translate_toplevels (tls : top_level list) =
  let top_level_fun_types = gen_top_level_types tls in
  let lx_toplevels = (List.map (fun tl ->
      match tl with
      | TLPolyAbs (name, _, cap_params, label_params, params, return_ty, body)
      | TLAbs (name, cap_params, label_params, params, return_ty, body) ->
        let fun_expr = SLsyntax.Fun { captured_set = None, Varset.empty; cap_params; label_params; params; return_ty; body } in
        let sl_typed = type_expr (Labels []) [] [] top_level_fun_types fun_expr in
        (match translate_SL sl_typed with
          | Fun (params', body') -> Syntax.TLAbs (name, params', body')
          | _ -> typing_error "Something went wrong\n"
        )
      | TLEffZSig (name, effect_sigs) -> TLEffZSig (name, List.map fst effect_sigs)
      | TLEffSig (name, effect_sigs) -> TLEffSig (name, effect_sigs) (* TODO *)
      | TLOpen f -> TLOpen f
      | TLOpenC f -> TLOpenC f
      | TLType defs -> TLType (List.map 
        (fun ({ type_name; type_params=_; type_cons }: SLsyntax.typedef) ->
          let type_cons' = List.map (fun (con, args) -> (con, List.map type_to_str args)) type_cons in
          { type_name; type_cons=type_cons' }
        ) defs)
    )
    tls) in
  lx_toplevels