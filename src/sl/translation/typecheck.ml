open SLsyntax
open Common
open Typed_ast
open Primitive

module Varset = Syntax__Varset

let effect_sigs_context = ref []
let effect_names: string list ref = ref []
let effectz_names: string list ref = ref[]
let type_defs_context = ref []

(* Evidence checking context *)
type region_ctx = {
  current_region : SLsyntax.region;
  evidence_env : (string * SLsyntax.region_constraint) list;
  label_regions : (string * SLsyntax.region) list;
}

let empty_region_ctx = { current_region = RTop; evidence_env = []; label_regions = [] }

let region_to_str r =
  match r with
  | RTop -> "⊤"
  | RVar v -> v
  | RNull -> "null"

let regions_eq r1 r2 =
  match r1, r2 with
  | RTop, RTop -> true
  | RVar v1, RVar v2 -> v1 = v2
  | RNull, RNull -> true
  | _ -> false

let rec check_evidence ev_env current_r target_r ev =
  match ev with
  | ENull -> ()
  | EZero ->
    if not (regions_eq current_r target_r) then
      typing_error "Do: Evidence 0 requires same region, but current region %s ≠ target region %s"
        (region_to_str current_r) (region_to_str target_r)
  | EVar v ->
    (match List.assoc_opt v ev_env with
    | Some { rc_left; rc_right; _ } ->
      if not (regions_eq rc_left current_r && regions_eq rc_right target_r) then
        typing_error "Do: Evidence variable %s has constraint %s ≤ %s, but need %s ≤ %s"
          v (region_to_str rc_left) (region_to_str rc_right)
          (region_to_str current_r) (region_to_str target_r)
    | None -> typing_error "Do: Evidence variable %s not found" v)
  | EPlus (e1, e2) ->
    let r_mid = infer_evidence_target ev_env current_r e1 in
    check_evidence ev_env r_mid target_r e2

and infer_evidence_target ev_env current_r ev =
  match ev with
  | ENull -> typing_error "Do: Cannot infer intermediate region from null evidence in +"
  | EZero -> current_r
  | EVar v ->
    (match List.assoc_opt v ev_env with
    | Some { rc_left; rc_right; _ } ->
      if not (regions_eq rc_left current_r) then
        typing_error "Do: Evidence variable %s starts at %s, but expected %s"
          v (region_to_str rc_left) (region_to_str current_r)
      else rc_right
    | None -> typing_error "Do: Evidence variable %s not found" v)
  | EPlus (e1, e2) ->
    let r_mid = infer_evidence_target ev_env current_r e1 in
    infer_evidence_target ev_env r_mid e2

(** Check for the presence of a capability variable. Raises an exception if not. *)
let check_cap_var var captured_vars cap_vars =
  if List.exists (fun cap -> var = cap) cap_vars then ()
  else
    match captured_vars with
      | Capability cap' -> if var = cap' then () else typing_error "Capability variable %s not found in context\n" var
      | _ -> typing_error "Capability variable %s not found in context\n" var

(** Checks the presence of all variables in a capability T. Raises an exception if not. *)
let check_capability (cap: capability) captured_vars cap_vars label_vars =
  let (cap_var, labels) = cap in
  let () = match cap_var with
    | Some cap -> check_cap_var cap captured_vars cap_vars
    | None -> ()
  in Varset.iter (fun label -> ignore(find_label_binding label captured_vars label_vars)) labels

(** Checks whether all elements in the capture set are present, and returns the same set as a capture_set.
Raises an exception if an element is not present or the set is ambiguous.*)
let check_capture_set_as_unamb (captured_set: capability) captured_vars cap_vars label_vars =
  check_capability captured_set captured_vars cap_vars label_vars;
  match captured_set with
  | Some cap, labels -> if not (Varset.is_empty labels) then
      typing_error "Captured set contains both a capability variable and a label\n"
    else Capability cap
  | None, labels ->
    let labels' = Varset.fold (fun label acc ->
      (label, find_label_binding label captured_vars label_vars)::acc
    ) labels [] in

    (* check effect name uniqueness for zero-cost effects *)
    let rec check_uniqueness effect_names =
      match effect_names with
      | [] -> true
      | name::rest -> if (List.exists ((=) name) !effectz_names) && (List.exists ((=) name) rest)
          then false
          else check_uniqueness rest
    in
    if check_uniqueness (List.map (fun (_, lb) -> lb.lb_effect_name) labels')
      then Labels labels'
      else typing_error "Captured set contains labels with duplicate zero-cost effect names\n"

(** Fetches the effect signature given the effect name and operation. *)
let get_effect_sig (effect_name: string) effect_op =
  let effect_sigs_list = List.assoc effect_name !effect_sigs_context in
  List.assoc effect_op effect_sigs_list

(** Look up the op_cty a handler associates with the given (label, op) pair. *)
let find_op_cty label op captured_vars label_vars : op_cty =
  let lb = find_label_binding label captured_vars label_vars in
  match List.assoc_opt op lb.lb_op_ctys with
  | Some o -> o
  | None ->
    typing_error "Do: Operation %s.%s not defined by the binding handler\n" label op

(** Builds a label_binding from a handler's signature + inferred ATM (C1, C2). *)
let make_label_binding sig_name c1 c2 : label_binding =
  let op_ctys =
    match List.assoc_opt sig_name !effect_sigs_context with
    | Some ops ->
      List.map (fun (op_name, (params_ty, return_ty)) ->
        (op_name, { op_params_ty = params_ty; op_return_ty = return_ty; op_c1 = c1; op_c2 = c2 })
      ) ops
    | None -> []
  in
  { lb_effect_name = sig_name; lb_op_ctys = op_ctys }

(** Checks whether expression e has value type ty. Effects are not constrained.
    Raises an error with expected and actual types if the value parts don't match. *)
let rec check_ty ?(msg = "") rctx captured_vars cap_vars label_vars term_vars (e: expr) ty =
  let te = type_expr rctx captured_vars cap_vars label_vars term_vars e in
  let actual = ty_of_cty te.expr_cty in
  if not (types_eq ty actual) then
    typing_error
      "%s\n\tExpected: %s\n\tActual: %s\n"
      msg
      (type_to_str ty)
      (type_to_str actual)
  else te

(** Checks an expression against an expected computation type, allowing the
    subsumption "pure ≤ any ATM" from the ott subtyping rules. If the expression
    is purely typed and the expected is impure, the resulting typed_expr is
    lifted to the expected cty. *)
and check_cty ?(msg = "") rctx captured_vars cap_vars label_vars term_vars (e: expr) expected =
  let te = type_expr rctx captured_vars cap_vars label_vars term_vars e in
  let actual = te.expr_cty in
  if ctys_eq actual expected then te
  else if cty_sub actual expected then { te with expr_cty = expected }
  else
    typing_error
      "%s\n\tExpected: %s\n\tActual: %s\n"
      msg
      (cty_to_str expected)
      (cty_to_str actual)

and type_expr rctx (captured_vars: capture_set) cap_vars label_vars (term_vars: (string * ty) list) (e: expr): typed_expr =
  let type_expr_with = type_expr in
  let check_cty_with = check_cty in
  let type_expr = type_expr rctx in
  let check_ty ?(msg="") = check_ty ~msg rctx in
  let check_cty ?(msg="") = check_cty ~msg rctx in
  let ty_of te = ty_of_cty te.expr_cty in
  let eff_of te = eff_of_cty te.expr_cty in
  let lift_to_cty ?(msg = "") te expected =
    if ctys_eq te.expr_cty expected then te
    else if cty_sub te.expr_cty expected then { te with expr_cty = expected }
    else
      typing_error
        "%s\n\tExpected: %s\n\tActual: %s\n"
        msg
        (cty_to_str expected)
        (cty_to_str te.expr_cty)
  in
  let common_eff ?(msg = "") eff1 eff2 =
    if effs_eq eff1 eff2 then eff1
    else if sub_eff eff1 eff2 then eff2
    else if sub_eff eff2 eff1 then eff1
    else
      typing_error
        "%s\n\tLeft: %s\n\tRight: %s\n"
        msg
        (eff_to_str eff1)
        (eff_to_str eff2)
  in
  let align_branch_ctys ?(msg = "") te1 te2 =
    let t1 = ty_of te1 in
    let t2 = ty_of te2 in
    if not (types_eq t1 t2) then
      typing_error
        "%s\n\tExpected: %s\n\tActual: %s\n"
        msg
        (type_to_str t1)
        (type_to_str t2)
    else
      let branch_cty = CCty (t1, common_eff ~msg (eff_of te1) (eff_of te2)) in
      ( lift_to_cty ~msg te1 branch_cty,
        lift_to_cty ~msg te2 branch_cty,
        branch_cty )
  in

  let expr_desc, expr_cty = match e with
    (* Basic types *)
    | Unit -> Unit, make_pure_cty TUnit
    | Int i -> Int i, make_pure_cty TInt
    | Float fl -> Float fl, make_pure_cty TFloat
    | Bool b -> Bool b, make_pure_cty TBool
    | Str s -> Str s, make_pure_cty TStr
    | Char c -> Char c, make_pure_cty TChar
    | Arith (e1, op, e2) ->
      let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TInt in
      let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TInt in
      let eff = compose_effs (eff_of e1') (eff_of e2') in
      Arith (e1', op, e2'), CCty (TInt, eff)
    | Cmp (e1, op, e2) ->
      if op == CEq || op == CNeq then
        let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
        let ty = ty_of e1' in
        let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 ty in
        let eff = compose_effs (eff_of e1') (eff_of e2') in
        Cmp (e1', op, e2'), CCty (TBool, eff)
      else
        let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TInt in
        let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TInt in
        let eff = compose_effs (eff_of e1') (eff_of e2') in
        Cmp (e1', op, e2'), CCty (TBool, eff)
    | Neg e ->
      let e' = check_ty captured_vars cap_vars label_vars term_vars e TBool in
      Neg e', CCty (TBool, eff_of e')
    | BArith (e1, op, e2) ->
      let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TBool in
      let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TBool in
      let eff = compose_effs (eff_of e1') (eff_of e2') in
      BArith (e1', op, e2'), CCty (TBool, eff)
    (* Reference Types *)
    | New el ->
      (match el with
        | [] -> New [], make_pure_cty (TRef TUnit)
        | e::rest ->
          let e' = type_expr captured_vars cap_vars label_vars term_vars e in
          let ty = ty_of e' in
          let rest' = List.map (fun e' -> check_ty captured_vars cap_vars label_vars term_vars e' ty ~msg:"New: The type of reference element doesn't match the first element's type") rest in
          let eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of e') rest' in
          New (e'::rest'), CCty (TRef ty, eff)
      )
    | Get (e, i) ->
      let e' = type_expr captured_vars cap_vars label_vars term_vars e in
      let t = ty_of e' in
      (match t with
        | TRef ty ->
          let i' = check_ty captured_vars cap_vars label_vars term_vars i TInt in
          let eff = compose_effs (eff_of e') (eff_of i') in
          Get(e', i'), CCty (ty, eff)
        | _ -> typing_error "Get: Expected a reference type"
      )
    | Set (e, i, nv) ->
      let e' = type_expr captured_vars cap_vars label_vars term_vars e in
      let t = ty_of e' in
      (match t with
        | TRef ty ->
          let i' = check_ty captured_vars cap_vars label_vars term_vars i TInt in
          let nv' = check_ty captured_vars cap_vars label_vars term_vars nv ty in
          let eff = compose_effs (compose_effs (eff_of e') (eff_of i')) (eff_of nv') in
          Set (e', i', nv'), CCty (TUnit, eff)
        | _ -> typing_error "Set: Expected a reference type"
      )

    | Var x ->
      (match List.assoc_opt x term_vars with
        | Some t -> Var x, make_pure_cty t
        | None -> typing_error "Var: Variable %s not found\n" x
      )

    | Stmt (e1, e2) ->
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let e2' = type_expr captured_vars cap_vars label_vars term_vars e2 in
      let eff = compose_effs (eff_of e1') (eff_of e2') in
      Stmt (e1', e2'), CCty (ty_of e2', eff)

    | Let (x, e1, e2) ->
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let t1 = ty_of e1' in
      let e2' = type_expr captured_vars cap_vars label_vars ((x, t1)::term_vars) e2 in
      let eff = compose_effs (eff_of e1') (eff_of e2') in
      Let (x, e1', e2'), CCty (ty_of e2', eff)

    | If (cond, e1, e2) ->
      let cond' = check_ty captured_vars cap_vars label_vars term_vars cond TBool ~msg:"If: Expected bool as condition" in
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let e2' = type_expr captured_vars cap_vars label_vars term_vars e2 in
      let e1'', e2'', branch_cty =
        align_branch_ctys ~msg:"If: The two branches don't agree" e1' e2'
      in
      let eff = compose_effs (eff_of cond') (eff_of_cty branch_cty) in
      If (cond', e1'', e2''), CCty (ty_of_cty branch_cty, eff)

    | Prim f ->
      let name = (String.sub f 1 ((String.length f) - 1)) in
      let ty = (
        match List.assoc_opt name prim_variable_args_sigs with
        | Some return_ty ->
          TFun {
            captured_set=None, Varset.empty;
            cap_params=[];
            label_params=[];
            params_ty=[];
            return_cty = make_pure_cty return_ty
          }
        | None ->
          (
            match List.assoc_opt name prim_polymophic_sigs with
            | Some (tv, params_ty, return_ty) ->
              TForall (tv, KTy,
                TFun {
                  captured_set=None, Varset.empty;
                  cap_params=[];
                  label_params=[];
                  params_ty=params_ty;
                  return_cty = make_pure_cty return_ty
                })
            | None ->
              let (params_ty, return_ty) = (
                match List.assoc_opt name prim_sigs with
                | Some f_sig -> f_sig
                | None -> ([], TInt) (* Default to int*)
              ) in
              TFun {
                captured_set=None, Varset.empty;
                cap_params=[];
                label_params=[];
                params_ty;
                return_cty = make_pure_cty return_ty
              }
          )
      ) in Prim f, make_pure_cty ty

    | Fun { captured_set; cap_params; label_params; params; body; return_cty } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      let params_ty = List.map snd params in
      (* The function body is typechecked at the annotated return_cty.
         The label_params are introduced with empty-op bindings (effectful function
         bodies that do-invoke label params are not yet supported). *)
      let inner_label_vars = List.map (fun (label, eff) ->
        (label, { lb_effect_name = eff; lb_op_ctys = [] })
      ) label_params in
      let body' = check_cty captured_vars' cap_params inner_label_vars (params@term_vars) body return_cty ~msg:"Fun: Function body doesn't match the declared return cty" in
      let ty = TFun {
        captured_set;
        cap_params;
        label_params;
        params_ty;
        return_cty
      } in
      Fun {
        captured_set;
        cap_params;
        label_params;
        params;
        body = body';
        return_cty;
      }, make_pure_cty ty

    | App { func; cap_insts; label_args; args = app_args } ->
      let func' = type_expr captured_vars cap_vars label_vars term_vars func in
      let fun_type = ty_of func' in
      (match fun_type with
        | TFun {captured_set; cap_params; label_params; params_ty; return_cty;_} ->
          (match func with
            | Prim f ->
              let name = (String.sub f 1 ((String.length f) - 1)) in
              (
                match List.assoc_opt name prim_variable_args_sigs with
                | Some return_ty ->
                  let app_args' = List.map (fun arg -> type_expr captured_vars cap_vars label_vars term_vars arg) app_args in
                  let eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of func') app_args' in
                  App { func = func'; cap_insts; label_args; args = app_args' }, CCty (return_ty, eff)
                | None ->
                  if List.exists (fun (prim_name, _) -> prim_name = name) prim_sigs
                    || List.exists (fun (prim_name, _) -> prim_name = name) prim_polymophic_sigs then
                    let app_args' = List.map2 (fun var ty -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args params_ty in
                    let eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of func') app_args' in
                    let return_ty = ty_of_cty return_cty in
                    App { func = func'; cap_insts; label_args; args = app_args' }, CCty (return_ty, eff)
                  else (* Primitive type info not found: Skip type checking arguments *)
                    let app_args' = List.map (fun var -> type_expr captured_vars cap_vars label_vars term_vars var) app_args in
                    let return_ty = ty_of_cty return_cty in
                    let eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of func') app_args' in
                    App { func = func'; cap_insts; label_args; args = app_args' }, CCty (return_ty, eff)

              )
            | _ ->
              let effect_names = List.map (fun label -> find_effect_name label captured_vars label_vars) label_args in
              check_capability captured_set captured_vars cap_vars label_vars;
              List.iter (fun cap_inst -> check_capability cap_inst captured_vars cap_vars label_vars) cap_insts;
              if not (effect_names = (List.map snd label_params)) then
                typing_error "App: Effect names for labels don't match\n\tExpected: %s\n\tActual: %s\n\n" (String.concat ", " (List.map snd label_params)) (String.concat ", " effect_names)
              else if not ((List.length cap_insts) = (List.length cap_params)) then
                typing_error "App: Capability length don't match\n"
              else
                let label_subs = List.map2 (fun x y -> (x, y)) (List.map fst label_params) label_args in
                let cap_subs = List.map2 (fun x y -> (x, y)) cap_params cap_insts in
                let args_ty_expected = List.map (fun ty -> substitute_to_type ty label_subs cap_subs) params_ty in
                if not ((List.length app_args) = (List.length args_ty_expected)) then
                  typing_error "App: Incorrect number of arguments\n\tExpected: %d\n\tActual:%d" (List.length app_args) (List.length args_ty_expected)
                else
                  let app_args' = List.map2 (fun var ty -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args args_ty_expected in
                  let substituted_cty = substitute_to_cty return_cty label_subs cap_subs in
                  let substituted_ty = ty_of_cty substituted_cty in
                  let substituted_eff = eff_of_cty substituted_cty in
                  let call_eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of func') app_args' in
                  let eff = compose_effs call_eff substituted_eff in
                  App { func = func'; cap_insts; label_args; args = app_args' }, CCty (substituted_ty, eff)
          )
        | _ -> typing_error "App: Function type expected\n\tActual: %s" (type_to_str fun_type)
      )

    | Handle { captured_set; region_binder; evidence_binder; handle_body; handler_label; sig_name; return_clause; handler_defs } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      let retarget_label_binding c1 c2 (lb: label_binding) =
        { lb with
          lb_op_ctys =
            List.map (fun (op_name, op_cty) ->
              (op_name, { op_cty with op_c1 = c1; op_c2 = c2 })
            ) lb.lb_op_ctys
        }
      in
      let retarget_capture_set c1 c2 captured_vars =
        match captured_vars with
        | Labels labels ->
          Labels (List.map (fun (label, lb) -> (label, retarget_label_binding c1 c2 lb)) labels)
        | Capability _ -> captured_vars
      in
      (* Use the explicit region binder for this handler's region. *)
      let handler_region = RVar region_binder in
      let handler_constraint = { rc_left = handler_region; rc_dist = DOne; rc_right = rctx.current_region } in
      let body_rctx = {
        current_region = handler_region;
        evidence_env = (region_binder, handler_constraint) :: (evidence_binder, handler_constraint) :: rctx.evidence_env;
        label_regions = (handler_label, handler_region) :: rctx.label_regions;
      } in
      (* Answer-type modification: the handle body has cty T / C1 => C2 where
         - T is the body's value type (given by the return clause's annotation when present),
         - C1 is the initial answer cty (return clause's output; also continuations' return cty),
         - C2 is the final answer cty (non-HDef op bodies' cty; also the handle expression's cty).
         Typecheck the handler first so that per-op ATMs are registered in the label
         binding before the body is checked. *)
      let body_ty, initial_ans_cty, typed_return_clause =
        match return_clause with
        | None ->
          (* No return clause. Preview the body using a placeholder ATM so that
             `Do` nodes can still be checked while we recover the body's value
             type. We then re-typecheck the body against the real inferred ATM. *)
          let placeholder_cty = CTyVar ("__ans_" ^ handler_label) in
          let preview_lb = make_label_binding sig_name placeholder_cty placeholder_cty in
          let preview_captured_vars = retarget_capture_set placeholder_cty placeholder_cty captured_vars' in
          let hb = type_expr_with body_rctx preview_captured_vars []
              [(handler_label, preview_lb)]
              term_vars handle_body in
          let t = ty_of hb in
          t, make_pure_cty t, None
        | Some { return_var; return_var_ty; return_body } ->
          let return_body' =
            type_expr_with body_rctx captured_vars' [] []
              ((return_var, return_var_ty) :: term_vars) return_body
          in
          let c1 = return_body'.expr_cty in
          ( return_var_ty,
            c1,
            Some ({ return_var; return_var_ty; return_cty = c1; return_body = return_body' }
              : Typed_ast.typed_return_clause) )
      in
      let final_ans_cty = ref None in
      let handler_defs' = List.map (fun ({ op_anno; op_name; op_params; op_body }: SLsyntax.hdl) ->
        let (effect_inputs_ty, effect_return_ty) = get_effect_sig sig_name op_name in
        let cont_type = TCont {
          captured_set;
          effect_return_ty;
          return_cty = initial_ans_cty
        } in
        let effect_inputs_ty = if (op_anno = HHdl1) || (op_anno = HHdls) then effect_inputs_ty@[cont_type] else effect_inputs_ty in
        if (List.length op_params) != (List.length effect_inputs_ty)
          then typing_error "Handle: Incorrect numbers of handler arguments\n\tExpected: %d\n\tActual: %d" (List.length effect_inputs_ty) (List.length op_params)
          else
            let handler_params = List.map2 (fun x y -> (x, y)) op_params effect_inputs_ty in
            let op_body' =
              if op_anno == HDef then
                type_expr_with body_rctx captured_vars' [] [] (handler_params@term_vars) op_body
              else
                match !final_ans_cty with
                | None ->
                  let te = type_expr_with body_rctx captured_vars' [] [] (handler_params@term_vars) op_body in
                  final_ans_cty := Some te.expr_cty;
                  te
                | Some c2 ->
                  check_cty_with ~msg:"Handle: Effect body cty doesn't agree on the final answer type"
                    body_rctx captured_vars' [] [] (handler_params@term_vars) op_body c2
            in
            { op_anno; op_name; op_params; op_body = op_body' }
      ) handler_defs in
      let c2 = match !final_ans_cty with
        | Some c2 -> c2
        | None -> initial_ans_cty
      in
      (* Register per-op types (including the inferred ATM) in the label binding,
         so that every `Do x.op [...]` inside the body can look up its C1, C2. *)
      let lb = make_label_binding sig_name initial_ans_cty c2 in
      let body_captured_vars = retarget_capture_set initial_ans_cty c2 captured_vars' in
      let body_label_vars = [(handler_label, lb)] in
      let body_expected_cty = CCty (body_ty, EAns (initial_ans_cty, c2)) in
      let handle_body' =
        check_cty_with ~msg:"Handle: Body doesn't match the handler's declared ATM"
          body_rctx body_captured_vars []
          body_label_vars term_vars
          handle_body body_expected_cty
      in
      Handle { captured_set; region_binder; evidence_binder; handle_body = handle_body'; handler_label; sig_name; return_clause = typed_return_clause; handler_defs = handler_defs' }, c2

    | Do { do_label; do_op; do_evidence; do_typelike_args; do_args } ->
      let op_cty_info = find_op_cty do_label do_op captured_vars label_vars in
      (* Check evidence: need Ev(current_region <= label_region) *)
      (match List.assoc_opt do_label rctx.label_regions with
      | Some target_region ->
        check_evidence rctx.evidence_env rctx.current_region target_region do_evidence
      | None -> ()
      );
      if (List.length do_args) != (List.length op_cty_info.op_params_ty)
        then typing_error "Do: Incorrect number of arguments\n\tExpected: %d\n\tActual: %d\n" (List.length op_cty_info.op_params_ty) (List.length do_args)
        else
          let do_args' = List.map2 (fun arg ty -> check_ty captured_vars cap_vars label_vars term_vars arg ty ~msg:"Do: Parameter types don't match") do_args op_cty_info.op_params_ty in
          let args_eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) EPure do_args' in
          let do_eff = EAns (op_cty_info.op_c1, op_cty_info.op_c2) in
          let eff = compose_effs args_eff do_eff in
          Do { do_label; do_op; do_evidence; do_typelike_args; do_args = do_args' }, CCty (op_cty_info.op_return_ty, eff)

    | Resume (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = ty_of cont' in
      (match type_cont with
      | TCont { effect_return_ty; return_cty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        let eff = compose_effs (compose_effs (eff_of cont') (eff_of arg')) (eff_of_cty return_cty) in
        Resume (cont', arg'), CCty (ty_of_cty return_cty, eff)
      | _ -> typing_error "Resume: Continuation type expected\n"
      )

    | ResumeFinal (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = ty_of cont' in
      (match type_cont with
      | TCont { effect_return_ty; return_cty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        let eff = compose_effs (compose_effs (eff_of cont') (eff_of arg')) (eff_of_cty return_cty) in
        ResumeFinal (cont', arg'), CCty (ty_of_cty return_cty, eff)
      | _ -> typing_error "Resume: Continuation type expected\n"
      )

    | Recdef (fundefs, e) ->
      let fun_vars = (List.map (fun ({ name; captured_set; cap_params; label_params; params; body=_; return_cty }: SLsyntax.fundef) ->
        let fun_ty = TFun {
          captured_set;
          cap_params;
          label_params;
          params_ty = List.map snd params;
          return_cty
        } in
        (name, fun_ty)) fundefs) in
      let fundefs' = List.map2 (fun ({ name; captured_set; cap_params; label_params; params; body; return_cty }: SLsyntax.fundef) fun_ty ->
        let fun_expr = SLsyntax.Fun { captured_set; cap_params; label_params; params; return_cty; body } in
        let fundef_expr = check_ty captured_vars cap_vars label_vars (fun_vars@term_vars) fun_expr fun_ty in
        match fundef_expr with
        | { expr_desc = Fun { body = body'; _ }; _} -> { name; captured_set; cap_params; label_params; params; body = body'; return_cty }
        | _ -> typing_error "Recdef: Incorrect typed_expr\n" (* Shouldn't happen *)
        ) fundefs (List.map snd fun_vars)
      in
      let e' = type_expr captured_vars cap_vars label_vars (fun_vars@term_vars) e in
      Recdef (fundefs', e'), e'.expr_cty

    | Typecon (t, t_args, args) ->
      (match List.find_opt (fun (_, (_, type_cons)) ->
          List.exists (fun (tcon, _) -> t = tcon) type_cons
        ) !type_defs_context with
        | None -> typing_error "Type Constructor %s not found" t
        | Some (type_name, (type_params, type_cons)) ->
          if (List.length type_params) != (List.length t_args)
            then typing_error "Type Constructor %s: Incorrect number of type arguments\n\tExpected: %d\n\tActual: %d" t (List.length type_params) (List.length t_args)
            else
              let type_con = List.assoc t type_cons in
              let type_subs = List.map2 (fun tv t_arg -> (tv, t_arg)) type_params t_args in
              let args' = List.map2 (fun arg ty ->
                check_ty captured_vars cap_vars label_vars term_vars arg (substitute_ty ty type_subs) ~msg:(Printf.sprintf "Type Constructor %s: Incorrect argument type" t)
              ) args type_con in
              let eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) EPure args' in
              Typecon (t, t_args, args'), CCty (TCon (type_name, t_args), eff)
      )

    | Match { match_expr; pattern_matching } ->
      let match_expr' = type_expr captured_vars cap_vars label_vars term_vars match_expr in
      let match_expr_ty = ty_of match_expr' in
      (match match_expr_ty with
        | TCon (t, t_args) ->
          let type_params, type_cons = List.assoc t !type_defs_context in
          if (List.length type_cons) > (List.length pattern_matching)
            then typing_error "Match: Not all patterns matched"
            else
              let type_subs = List.map2 (fun tv t_arg -> (tv, t_arg)) type_params t_args in
              (match pattern_matching with
                | (pt, res)::pattern_matching_rest ->
                  (match pt with
                    | PTypecon (type_con, args) ->
                      let params_type = List.map (fun ty -> substitute_ty ty type_subs) (List.assoc type_con type_cons) in
                      let arg_vars = List.map2 (fun arg ty -> (arg, ty)) args params_type in
                      let res' = type_expr captured_vars cap_vars label_vars (arg_vars@term_vars) res in
                      let res_ty = ty_of res' in
                      let pattern_matching_rest' = List.map (fun (pt, res) -> (match pt with
                      | Syntax__Common.PTypecon (type_con, args) ->
                        let params_type = List.map (fun ty -> substitute_ty ty type_subs) (List.assoc type_con type_cons) in
                        let arg_vars = List.map2 (fun arg ty -> (arg, ty)) args params_type in
                        let res' = check_ty captured_vars cap_vars label_vars (arg_vars@term_vars) res res_ty ~msg:"Match: The types of clauses don't match" in
                        (pt, res')
                      )) pattern_matching_rest in
                      let eff = compose_effs (eff_of match_expr') (eff_of res') in
                      Match { match_expr = match_expr'; pattern_matching = (pt, res')::pattern_matching_rest' }, CCty (res_ty, eff)
                  )
                | _ -> Match { match_expr = match_expr'; pattern_matching = [] }, make_pure_cty TUnit
              )
        | _ -> typing_error "Match: Pattern type expected, got %s instead" (type_to_str match_expr_ty)
      )
    | TypeApp (t_arg, e) ->
      let te = type_expr captured_vars cap_vars label_vars term_vars e in
      let te_ty = ty_of te in
      let instantiated_ty = (match te_ty with
        | TForall (tv, _kind, t') ->
          substitute_ty t' [(tv, t_arg)]
        | _ -> typing_error "TypeApp: A forall type expected\n\tActual: %s" (type_to_str te_ty)
      ) in
      TypeApp (t_arg, te), CCty (instantiated_ty, eff_of te)


  in
  { expr_desc; expr_cty; captured_vars; cap_vars; label_vars }


let check_type_defs (defs: typedef list) =
  let type_names' = (List.map fst !type_defs_context)@(List.map (fun { type_name; _ } -> type_name) defs) in

  let rec check_typedef_ty type_names type_vars ty =
    match ty with
    | TRef ty' -> check_typedef_ty type_names type_vars ty'
    | TFun { params_ty; return_cty; _ } ->
      List.iter (check_typedef_ty type_names type_vars) params_ty;
      check_typedef_cty type_names type_vars return_cty
    | TCont { effect_return_ty; return_cty; _ } ->
      check_typedef_ty type_names type_vars effect_return_ty;
      check_typedef_cty type_names type_vars return_cty
    | TNode ty' -> check_typedef_ty type_names type_vars ty'
    | TTree ty' -> check_typedef_ty type_names type_vars ty'
    | TQueue ty' -> check_typedef_ty type_names type_vars ty'
    | TArray ty' -> check_typedef_ty type_names type_vars ty'
    | TCon (tn, t_args) ->
      if List.exists (fun name -> name = tn) type_names
        then List.iter (check_typedef_ty type_names type_vars) t_args
        else typing_error "Type Definition: type %s not found" tn
    | TVar tv ->
      if List.exists (fun tv' -> tv = tv') type_vars
        then () else typing_error "Type Definition: type variable %s not defined" tv
    | TForall (tv, _kind, ty') -> check_typedef_ty type_names (tv::type_vars) ty'
    | _ -> ()
  and check_typedef_cty type_names type_vars cty =
    match cty with
    | CTyVar _ -> ()
    | CCty (ty, eff) ->
      check_typedef_ty type_names type_vars ty;
      check_typedef_eff type_names type_vars eff
    | CFill (_, cty') -> check_typedef_cty type_names type_vars cty'
  and check_typedef_eff type_names type_vars eff =
    match eff with
    | EPure -> ()
    | EAns (c1, c2) ->
      check_typedef_cty type_names type_vars c1;
      check_typedef_cty type_names type_vars c2
    | EEffVar _ -> ()
  in

  List.iter (fun { type_name=_; type_params; type_cons } ->
    List.iter (fun (_, args_ty) ->
      List.iter (check_typedef_ty type_names' type_params) args_ty
    ) type_cons
  ) defs
