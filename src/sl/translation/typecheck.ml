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

let fresh_region_counter = ref 0
let fresh_region () =
  let n = !fresh_region_counter in
  fresh_region_counter := n + 1;
  RVar (Printf.sprintf "__r%d" n)

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
  in Varset.iter (fun label -> ignore(find_effect_name label captured_vars label_vars)) labels

(** Checks whether all elements in the capture set are present, and returns the same set as a capture_set. 
Raises an exception if an element is not present or the set is ambiguous.*)
let check_capture_set_as_unamb (captured_set: capability) captured_vars cap_vars label_vars =
  check_capability captured_set captured_vars cap_vars label_vars;
  match captured_set with
  | Some cap, labels -> if not (Varset.is_empty labels) then
      typing_error "Captured set contains both a capability variable and a label\n"
    else Capability cap
  | None, labels ->
    let labels' = Varset.fold (fun label acc -> (label, find_effect_name label captured_vars label_vars)::acc) labels [] in

    (* check effect name uniqueness for zero-cost effects *)
    let rec check_uniqueness effect_names = 
      match effect_names with
      | [] -> true
      | name::rest -> if (List.exists ((=) name) !effectz_names) && (List.exists ((=) name) rest) 
          then false 
          else check_uniqueness rest
    in
    if check_uniqueness (List.map snd labels') 
      then Labels labels'
      else typing_error "Captured set contains labels with duplicate zero-cost effect names\n"

(** Fetches the effect signature given the effect name and operation. *)
let get_effect_sig (effect_name: string) effect_op =
  let effect_sigs_list = List.assoc effect_name !effect_sigs_context in
  List.assoc effect_op effect_sigs_list

(** Checks whether expression e has type ty. Raises an error with expected and actual types if not. *)
let rec check_ty ?(msg = "") rctx captured_vars cap_vars label_vars term_vars (e: expr) ty =
  let te = type_expr rctx captured_vars cap_vars label_vars term_vars e in
  let { expr_ty=ty'; _ } = te in
    if not (types_eq ty ty') then
      typing_error
        "%s\n\tExpected: %s\n\tActual: %s\n"
        msg
        (type_to_str ty)
        (type_to_str ty')
    else te

and type_expr rctx (captured_vars: capture_set) cap_vars label_vars (term_vars: (string * ty) list) (e: expr): typed_expr =
  let type_expr_with = type_expr in
  let type_expr = type_expr rctx in
  let check_ty ?(msg="") = check_ty ~msg rctx in
  let type_of te = 
    let { expr_ty; _ } = te in expr_ty in

  let expr_desc, expr_ty = match e with
    (* Basic types *)
    | Unit -> Unit, TUnit
    | Int i -> Int i, TInt
    | Float fl -> Float fl, TFloat
    | Bool b -> Bool b, TBool
    | Str s -> Str s, TStr
    | Char c -> Char c, TChar
    | Arith (e1, op, e2) -> 
      let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TInt in
      let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TInt in (*TODO: Add float types*)
      Arith (e1', op, e2'), TInt
    | Cmp (e1, op, e2) ->
      if op == CEq || op == CNeq then
        let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
        let ty = type_of e1' in
        let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 ty in
        Cmp (e1', op, e2'), TBool
      else
        let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TInt in
        let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TInt in
        Cmp (e1', op, e2'), TBool
    | Neg e -> 
      let e' = check_ty captured_vars cap_vars label_vars term_vars e TBool in
      Neg e', TBool
    | BArith (e1, op, e2) ->
      let e1' = check_ty captured_vars cap_vars label_vars term_vars e1 TBool in
      let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 TBool in
      BArith (e1', op, e2'), TBool
    (* Reference Types *)
    | New el -> 
      (match el with
        | [] -> New [], TRef TUnit
        | e::rest ->
          let e' = type_expr captured_vars cap_vars label_vars term_vars e in
          let ty = type_of e' in
          let rest' = List.map (fun e' -> check_ty captured_vars cap_vars label_vars term_vars e' ty ~msg:"New: The type of reference element doesn't match the first element's type") rest in
          New (e'::rest'), TRef ty
      )
    | Get (e, i) ->
      let e' = type_expr captured_vars cap_vars label_vars term_vars e in
      let t = type_of e' in
      (match t with
        | TRef ty -> 
          let i' = check_ty captured_vars cap_vars label_vars term_vars i TInt in
          Get(e', i'), ty
        | _ -> typing_error "Get: Expected a reference type"
      )
    | Set (e, i, nv) ->
      let e' = type_expr captured_vars cap_vars label_vars term_vars e in
      let t = type_of e' in
      (match t with
        | TRef ty -> 
          let i' = check_ty captured_vars cap_vars label_vars term_vars i TInt in
          let nv' = check_ty captured_vars cap_vars label_vars term_vars nv ty in
          Set (e', i', nv'), TUnit
        | _ -> typing_error "Set: Expected a reference type"
      )
    
    | Var x -> 
      (match List.assoc_opt x term_vars with
        | Some t -> Var x, t
        | None -> typing_error "Var: Variable %s not found\n" x
      )

    | Stmt (e1, e2) ->
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let e2' = type_expr captured_vars cap_vars label_vars term_vars e2 in
      let ty = type_of e2' in
      Stmt (e1', e2'), ty

    | Let (x, e1, e2) ->
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let t1 = type_of e1' in
      let e2' = type_expr captured_vars cap_vars label_vars ((x, t1)::term_vars) e2 in
      let t2 = type_of e2' in
      Let (x, e1', e2'), t2

    | If (cond, e1, e2) ->
      let cond' = check_ty captured_vars cap_vars label_vars term_vars cond TBool ~msg:"If: Expected bool as condition" in
      let e1' = type_expr captured_vars cap_vars label_vars term_vars e1 in
      let t1 = type_of e1' in
      let e2' = check_ty captured_vars cap_vars label_vars term_vars e2 t1 ~msg:"If: The types of two branches don't match" in
      If (cond', e1', e2'), t1

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
            return_ty
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
                  return_ty
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
                return_ty
              }
          )
      ) in Prim f, ty

    | Fun { captured_set; cap_params; label_params; params; body; return_ty } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      let params_ty = List.map snd params in
      let body' = check_ty captured_vars' cap_params label_params (params@term_vars) body return_ty ~msg:"Fun: Function return type doesn't match" in
      let ty = TFun {
        captured_set;
        cap_params;
        label_params;
        params_ty;
        return_ty
      } in
      Fun {
        captured_set;
        cap_params;
        label_params;
        params;
        body = body';
        return_ty;
      }, ty

    | App { func; cap_insts; label_args; args = app_args } -> 
      let func' = type_expr captured_vars cap_vars label_vars term_vars func in
      let fun_type = type_of func' in
      (match fun_type with
        | TFun {captured_set; cap_params; label_params; params_ty; return_ty;_} -> 
          (match func with
            | Prim f -> 
              let name = (String.sub f 1 ((String.length f) - 1)) in
              (
                match List.assoc_opt name prim_variable_args_sigs with
                | Some return_ty -> 
                  let app_args' = List.map (fun arg -> type_expr captured_vars cap_vars label_vars term_vars arg) app_args in
                  App { func = func'; cap_insts; label_args; args = app_args' }, return_ty
                | None -> 
                  if List.exists (fun (prim_name, _) -> prim_name = name) prim_sigs
                    || List.exists (fun (prim_name, _) -> prim_name = name) prim_polymophic_sigs then
                    let app_args' = List.map2 (fun var ty -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args params_ty in 
                    App { func = func'; cap_insts; label_args; args = app_args' }, return_ty
                  else (* Primitive type info not found: Skip type checking arguments *)
                    let app_args' = List.map (fun var -> type_expr captured_vars cap_vars label_vars term_vars var) app_args in 
                    App { func = func'; cap_insts; label_args; args = app_args' }, return_ty

              )
            | _ ->
              let effect_names = List.map (fun label -> find_effect_name label captured_vars label_vars) label_args in
              (* Check if the captured set is valid *)
              check_capability captured_set captured_vars cap_vars label_vars;
              (* Check if the capability arguments are valid *)
              List.iter (fun cap_inst -> check_capability cap_inst captured_vars cap_vars label_vars) cap_insts;
              (* Check capability and label arguments *)
              if not (effect_names = (List.map snd label_params)) then 
                typing_error "App: Effect names for labels don't match\n\tExpected: %s\n\tActual: %s\n\n" (String.concat ", " (List.map snd label_params)) (String.concat ", " effect_names)
              else if not ((List.length cap_insts) = (List.length cap_params)) then
                typing_error "App: Capability length don't match\n"
              else
                (* Check argument types *)
                let label_subs = List.map2 (fun x y -> (x, y)) (List.map fst label_params) label_args in
                let cap_subs = List.map2 (fun x y -> (x, y)) cap_params cap_insts in
                let args_ty_expected = List.map (fun ty -> substitute_to_type ty label_subs cap_subs) params_ty in
                if not ((List.length app_args) = (List.length args_ty_expected)) then
                  typing_error "App: Incorrect number of arguments\n\tExpected: %d\n\tActual:%d" (List.length app_args) (List.length args_ty_expected)
                else
                  let app_args' = List.map2 (fun var ty -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args args_ty_expected in
                  let ty = substitute_to_type return_ty label_subs cap_subs in
                  App { func = func'; cap_insts; label_args; args = app_args' }, ty
          )
        | _ -> typing_error "App: Function type expected\n\tActual: %s" (type_to_str fun_type)
      )

    | Handle { captured_set; handle_body; handler_label; sig_name; return_clause; handler_defs } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      (* Introduce fresh region for this handler *)
      let handler_region = fresh_region () in
      let body_rctx = {
        current_region = handler_region;
        evidence_env = (handler_label, { rc_left = handler_region; rc_dist = DOne; rc_right = rctx.current_region }) :: rctx.evidence_env;
        label_regions = (handler_label, handler_region) :: rctx.label_regions;
      } in
      let handle_body' = type_expr_with body_rctx captured_vars' [] [(handler_label, sig_name)] term_vars handle_body in
      let body_ty = type_of handle_body' in
      (* If there's a return clause, type-check it to get the answer type;
         otherwise the answer type equals the body type. *)
      let answer_ty, typed_return_clause = match return_clause with
        | None -> body_ty, None
        | Some { return_var; return_body } ->
          let return_body' = type_expr captured_vars cap_vars label_vars ((return_var, body_ty)::term_vars) return_body in
          let ans_ty = type_of return_body' in
          ans_ty, Some ({ return_var; return_body = return_body' }: Typed_ast.typed_return_clause)
      in
      let handler_defs' = List.map (fun ({ op_anno; op_name; op_params; op_body }: SLsyntax.hdl) ->
        let (effect_inputs_ty, effect_return_ty) = get_effect_sig sig_name op_name in
        let cont_type = TCont {
          captured_set;
          effect_return_ty;
          return_ty = answer_ty
        } in
        let effect_inputs_ty = if (op_anno = HHdl1) || (op_anno = HHdls) then effect_inputs_ty@[cont_type] else effect_inputs_ty in
        if (List.length op_params) != (List.length effect_inputs_ty) 
          then typing_error "Handle: Incorrect numbers of handler arguments\n\tExpected: %d\n\tActual: %d" (List.length effect_inputs_ty) (List.length op_params) 
          else
            let handler_params = List.map2 (fun x y -> (x, y)) op_params effect_inputs_ty in
            let op_body' = if op_anno == HDef 
              then type_expr captured_vars' [] [] (handler_params@term_vars) op_body
              else check_ty captured_vars' [] [] (handler_params@term_vars) op_body answer_ty ~msg:"Handle: Effect body type doesn't match answer type"
            in
            { op_anno; op_name; op_params; op_body = op_body' }
      ) handler_defs in
      Handle { captured_set; handle_body = handle_body'; handler_label; sig_name; return_clause = typed_return_clause; handler_defs = handler_defs' }, answer_ty

    | Do { do_label; do_op; do_evidence; do_typelike_args; do_args } ->
      let effect_name = find_effect_name do_label captured_vars label_vars in
      let (effect_params_ty, effect_return_ty) = get_effect_sig effect_name do_op in
      (* Check evidence: need Ev(current_region <= label_region) *)
      (match List.assoc_opt do_label rctx.label_regions with
      | Some target_region ->
        check_evidence rctx.evidence_env rctx.current_region target_region do_evidence
      | None -> ()
      );
      if (List.length do_args) != (List.length effect_params_ty) 
        then typing_error "Do: Incorrect number of arguments\n\tExpected: %d\n\tActual: %d\n" (List.length effect_params_ty) (List.length do_args)
        else 
          let do_args' = List.map2 (fun arg ty -> check_ty captured_vars cap_vars label_vars term_vars arg ty ~msg:"Do: Parameter types don't match") do_args effect_params_ty in
          Do { do_label; do_op; do_evidence; do_typelike_args; do_args = do_args' }, effect_return_ty

    | Resume (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = type_of cont' in 
      (match type_cont with
      | TCont { effect_return_ty; return_ty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        Resume (cont', arg'), return_ty
      | _ -> typing_error "Resume: Continuation type expected\n"
      )
    
    | ResumeFinal (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = type_of cont' in 
      (match type_cont with
      | TCont { effect_return_ty; return_ty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        ResumeFinal (cont', arg'), return_ty
      | _ -> typing_error "Resume: Continuation type expected\n"
      )

    | Recdef (fundefs, e) -> 
      let fun_vars = (List.map (fun ({ name; captured_set; cap_params; label_params; params; body=_; return_ty }: SLsyntax.fundef) ->
        let fun_ty = TFun {
          captured_set;
          cap_params;
          label_params;
          params_ty = List.map snd params;
          return_ty
        } in 
        (name, fun_ty)) fundefs) in
      let fundefs' = List.map2 (fun ({ name; captured_set; cap_params; label_params; params; body; return_ty }: SLsyntax.fundef) fun_ty ->
        let fun_expr = SLsyntax.Fun { captured_set; cap_params; label_params; params; return_ty; body } in
        let fundef_expr = check_ty captured_vars cap_vars label_vars (fun_vars@term_vars) fun_expr fun_ty in
        match fundef_expr with 
        | { expr_desc = Fun { body = body'; _ }; _} -> { name; captured_set; cap_params; label_params; params; body = body'; return_ty }
        | _ -> typing_error "Recdef: Incorrect typed_expr\n" (* Shouldn't happen *)
        ) fundefs (List.map snd fun_vars) 
      in
      let e' = type_expr captured_vars cap_vars label_vars (fun_vars@term_vars) e in
      let ty = type_of e' in
      Recdef (fundefs', e'), ty

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
              Typecon (t, t_args, args'), TCon (type_name, t_args)
      )

    | Match { match_expr; pattern_matching } ->
      let match_expr' = type_expr captured_vars cap_vars label_vars term_vars match_expr in
      let match_expr_ty = type_of match_expr' in
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
                      let res_ty = type_of res' in
                      let pattern_matching_rest' = List.map (fun (pt, res) -> (match pt with
                      | Syntax__Common.PTypecon (type_con, args) -> 
                        let params_type = List.map (fun ty -> substitute_ty ty type_subs) (List.assoc type_con type_cons) in
                        let arg_vars = List.map2 (fun arg ty -> (arg, ty)) args params_type in
                        let res' = check_ty captured_vars cap_vars label_vars (arg_vars@term_vars) res res_ty ~msg:"Match: The types of clauses don't match" in
                        (pt, res')
                      )) pattern_matching_rest in
                      Match { match_expr = match_expr'; pattern_matching = (pt, res')::pattern_matching_rest' }, res_ty
                  )
                | _ -> Match { match_expr = match_expr'; pattern_matching = [] }, TUnit
              )
        | _ -> typing_error "Match: Pattern type expected, got %s instead" (type_to_str match_expr_ty)
      )
    | TypeApp (t_arg, e) ->
      let te = type_expr captured_vars cap_vars label_vars term_vars e in
      let te_ty = type_of te in
      let instantiated_ty = (match te_ty with 
        | TForall (tv, _kind, t') -> 
          substitute_ty t' [(tv, t_arg)]
        | _ -> typing_error "TypeApp: A forall type expected\n\tActual: %s" (type_to_str te_ty)
      ) in
      TypeApp (t_arg, te), instantiated_ty


  in
  { expr_desc; expr_ty; captured_vars; cap_vars; label_vars }


let check_type_defs (defs: typedef list) =
  let type_names' = (List.map fst !type_defs_context)@(List.map (fun { type_name; _ } -> type_name) defs) in
  
  let rec check_typedef_ty type_names type_vars ty = 
    match ty with
    | TRef ty' -> check_typedef_ty type_names type_vars ty'
    | TFun { params_ty; return_ty; _ } -> 
      List.iter (check_typedef_ty type_names type_vars) params_ty;
      check_typedef_ty type_names type_vars return_ty
    | TCont { effect_return_ty; return_ty; _ } ->
      check_typedef_ty type_names type_vars effect_return_ty;
      check_typedef_ty type_names type_vars return_ty
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
    | _ -> () in

  List.iter (fun { type_name=_; type_params; type_cons } ->
    List.iter (fun (_, args_ty) -> 
      List.iter (check_typedef_ty type_names' type_params) args_ty
    ) type_cons
  ) defs