open SLsyntax
open Common
open Typed_ast
open Primitive

module Varset = Syntax__Varset

let effect_sigs_context = ref []
let effect_names: string list ref = ref []
let effectz_names: string list ref = ref[]
let type_defs_context = ref []

(* Region checking context *)
type region_ctx = {
  (* The region the expression currently being checked sits in. Starts at
     RTop at the top level and is replaced by the handler's region binder
     inside a Handle body, so that Raise can form the constraint
     current_region ≤ target_region against the subregion tree. *)
  current_region : SLsyntax.region;
  (* Concrete subregion edges introduced by nested handlers. Each pair
     (inner, outer) represents inner ⊏ outer in the current derivation. *)
  subregions : (SLsyntax.region * SLsyntax.region) list;
  (* Maps a handler label to the region it was introduced at. Raise looks up
     its raise_label here to recover the target region for the subregion
     check; new entries are added as Handle expressions are entered. *)
  label_regions : (string * SLsyntax.region) list;
  (* Region variables whose concrete position is being inferred. A function body
     is checked under a fresh flexible region; obligations that can only be
     completed once that region is known are kept as residual constraints. *)
  flex_regions : SLsyntax.region list;
  (* Kinds of in-scope type-level variables. Currently only KATC entries
     are consulted (by check_atc); other kinds may be stored as a record
     of what the binder declared. Populated when type abstractions are
     introduced. *)
  kind_env : (string * SLsyntax.kind) list;
}

let empty_region_ctx = { current_region = RTop; subregions = []; label_regions = []; flex_regions = []; kind_env = [] }

type answer_context = {
  initial_binder : string option;
  initial_cty : SLsyntax.cty;
}

let fresh_refinement_witness =
  let counter = ref 0 in
  fun () ->
    let name = "__refine_witness_" ^ string_of_int !counter ^ "__" in
    incr counter;
    name

let fresh_path_hypothesis =
  let counter = ref 0 in
  fun () ->
    let name = "__path_hyp_" ^ string_of_int !counter ^ "__" in
    incr counter;
    name

let fresh_function_region =
  let counter = ref 0 in
  fun () ->
    let name = "__fun_region_" ^ string_of_int !counter ^ "__" in
    incr counter;
    RVar name

let fresh_handler_answer_var =
  let counter = ref 0 in
  fun () ->
    let name = "__handler_answer_" ^ string_of_int !counter ^ "__" in
    incr counter;
    name

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

let constraints_union c1 c2 =
  List.fold_left
    (fun acc c -> if List.exists ((=) c) acc then acc else c :: acc)
    c1 c2

let constraints_unions constraints =
  List.fold_left constraints_union [] constraints

let region_nodes subregions =
  List.fold_left
    (fun acc (inner, outer) -> constraints_union acc [inner; outer])
    [RTop] subregions

let region_is_node subregions region =
  List.exists (regions_eq region) (region_nodes subregions)

let region_is_flexible rctx region =
  List.exists (regions_eq region) rctx.flex_regions

let rec resolve_subregion_path subregions current_r target_r visited =
  if regions_eq current_r target_r then Some DZero
  else if List.exists (regions_eq current_r) visited then None
  else
    let next_edges =
      List.filter (fun (inner, _) -> regions_eq inner current_r) subregions
    in
    let rec try_edges = function
      | [] -> None
      | (_, outer) :: rest ->
        match resolve_subregion_path subregions outer target_r (current_r :: visited) with
        | Some d -> Some (DPlus (DOne, d))
        | None -> try_edges rest
    in
    try_edges next_edges

let rec distance_atoms = function
  | DZero -> []
  | DPlus (d1, d2) -> distance_atoms d1 @ distance_atoms d2
  | d -> [d]

let distance_of_atoms = function
  | [] -> DZero
  | d :: rest -> List.fold_left (fun acc x -> DPlus (acc, x)) d rest

let drop_distance_prefix prefix distance =
  let rec ones_count d =
    match normalize_distance d with
    | DZero -> Some 0
    | DOne -> Some 1
    | DPlus (DOne, rest) ->
      Option.map ((+) 1) (ones_count rest)
    | _ -> None
  in
  match ones_count prefix with
  | None -> Some distance
  | Some count ->
    let rec drop n atoms =
      if n = 0 then Some atoms
      else
        match atoms with
        | DOne :: rest -> drop (n - 1) rest
        | [] -> None
        | _ :: _ -> Some atoms
    in
    Option.map distance_of_atoms (drop count (distance_atoms (normalize_distance distance)))

type subregion_resolution =
  | Complete of distance
  | Blocked of region * distance

let rec resolve_subregion_or_flex rctx current_r target_r visited =
  if regions_eq current_r target_r then Some (Complete DZero)
  else if List.exists (regions_eq current_r) visited then None
  else if region_is_flexible rctx current_r then Some (Blocked (current_r, DZero))
  else
    let next_edges =
      List.filter (fun (inner, _) -> regions_eq inner current_r) rctx.subregions
    in
    let rec try_edges blocked = function
      | [] -> blocked
      | (_, outer) :: rest ->
        match resolve_subregion_or_flex rctx outer target_r (current_r :: visited) with
        | Some (Complete d) -> Some (Complete (DPlus (DOne, d)))
        | Some (Blocked (flex_region, d)) ->
          let blocked = Some (Blocked (flex_region, DPlus (DOne, d))) in
          try_edges blocked rest
        | None -> try_edges blocked rest
    in
    try_edges None next_edges

let check_subregion_constraint rctx { rc_inner; rc_dist; rc_outer } =
  match resolve_subregion_or_flex rctx rc_inner rc_outer [] with
  | Some (Complete d) ->
    if distance_eq d rc_dist then []
    else
      typing_error
        "Do: subregion path %s <= %s has level %s, but ATC has level %s"
        (region_to_str rc_inner)
        (region_to_str rc_outer)
        (distance_to_str d)
        (distance_to_str rc_dist)
  | Some (Blocked (flex_region, prefix)) ->
    (match drop_distance_prefix prefix rc_dist with
     | Some residual_dist ->
       [{ rc_inner = flex_region; rc_dist = residual_dist; rc_outer }]
     | None ->
       typing_error
         "Do: subregion path from %s already has level %s, but ATC has level %s"
         (region_to_str rc_inner)
         (distance_to_str prefix)
         (distance_to_str rc_dist))
  | None ->
    let inner_node = region_is_node rctx.subregions rc_inner in
    let outer_node = region_is_node rctx.subregions rc_outer in
    if inner_node && outer_node then
      typing_error
        "Do: no subregion path from %s to %s"
        (region_to_str rc_inner)
        (region_to_str rc_outer)
    else
      [{ rc_inner; rc_dist; rc_outer }]

let check_subregion rctx current_r target_r distance =
  check_subregion_constraint rctx
    { rc_inner = current_r; rc_dist = distance; rc_outer = target_r }

let check_region_requirement rctx current_r required_r =
  if regions_eq required_r RTop then []
  else
    match resolve_subregion_or_flex rctx current_r required_r [] with
    | Some (Complete _) -> []
    | Some (Blocked (flex_region, _)) ->
      [{ rc_inner = flex_region; rc_dist = DZero; rc_outer = required_r }]
    | None ->
      let current_node = region_is_node rctx.subregions current_r in
      let required_node = region_is_node rctx.subregions required_r in
      if current_node && required_node then
        typing_error
          "App: current region %s does not satisfy function region requirement %s"
          (region_to_str current_r)
          (region_to_str required_r)
      else
        [{ rc_inner = current_r; rc_dist = DZero; rc_outer = required_r }]

let region_requirement_sub rctx actual_req expected_req =
  (* Function regions are preconditions. An actual function can stand in for an
     expected one when it accepts every region the expected type promises. *)
  regions_eq actual_req expected_req
  || regions_eq actual_req RTop
  || Option.is_some (resolve_subregion_path rctx.subregions expected_req actual_req [])

let infer_function_region rctx fun_region constraints =
  let is_fun_region = regions_eq fun_region in
  let fun_constraints, residual =
    List.partition
      (fun c -> is_fun_region c.rc_inner || is_fun_region c.rc_outer)
      constraints
  in
  let candidates =
    List.filter_map
      (fun c ->
        if is_fun_region c.rc_inner then
          if is_fun_region c.rc_outer then None else Some c.rc_outer
        else
          typing_error
            "Fun: cannot infer invocation region from residual constraint %s <= %s"
            (region_to_str c.rc_inner)
            (region_to_str c.rc_outer))
      fun_constraints
  in
  let choose_stricter r1 r2 =
    if regions_eq r1 r2 then r1
    else if regions_eq r1 RTop then r2
    else if regions_eq r2 RTop then r1
    else
      match resolve_subregion_path rctx.subregions r1 r2 [],
            resolve_subregion_path rctx.subregions r2 r1 [] with
      | Some _, _ -> r1
      | _, Some _ -> r2
      | None, None ->
        typing_error
          "Fun: cannot choose a single invocation region from incomparable requirements %s and %s"
          (region_to_str r1)
          (region_to_str r2)
  in
  let inferred_region =
    List.fold_left choose_stricter RTop candidates
  in
  List.iter
    (fun c ->
      if is_fun_region c.rc_inner
         && not (region_requirement_sub rctx inferred_region c.rc_outer)
      then
        typing_error
          "Fun: inferred region %s does not satisfy residual requirement %s"
          (region_to_str inferred_region)
          (region_to_str c.rc_outer))
    fun_constraints;
  inferred_region, residual

let rec fv_distance = function
  | DVar v -> [v]
  | DPlus (d1, d2) -> constraints_union (fv_distance d1) (fv_distance d2)
  | DZero | DOne -> []

let fv_region = function
  | RVar v -> [v]
  | RTop | RNull -> []

let fv_region_constraint { rc_inner; rc_dist; rc_outer } =
  constraints_unions [fv_region rc_inner; fv_distance rc_dist; fv_region rc_outer]

let constraint_mentions v c =
  List.exists ((=) v) (fv_region_constraint c)

let partition_constraints_by_var v constraints =
  List.partition (constraint_mentions v) constraints

let close_forall_constraints bindings ty constraints =
  List.fold_right
    (fun (tv, kind) (ty, constraints) ->
      let captured, residual = partition_constraints_by_var tv constraints in
      TForall (tv, kind, captured, ty), residual)
    bindings (ty, constraints)

let substitute_tylike_to_region region var kind arg =
  match kind, arg with
  | KReg, _ ->
    (match region_of_tylike arg with
     | Some replacement ->
       (match region with
        | RVar v when v = var -> replacement
        | _ -> region)
     | None -> region)
  | _ -> region

let rec substitute_tylike_to_distance distance var kind arg =
  match kind, arg with
  | KDist, TLDist replacement ->
    (match distance with
     | DVar v when v = var -> replacement
     | DPlus (d1, d2) ->
       DPlus (
         substitute_tylike_to_distance d1 var kind arg,
         substitute_tylike_to_distance d2 var kind arg
       )
     | _ -> distance)
  | _ ->
    (match distance with
     | DPlus (d1, d2) ->
       DPlus (
         substitute_tylike_to_distance d1 var kind arg,
         substitute_tylike_to_distance d2 var kind arg
       )
     | _ -> distance)

let substitute_tylike_to_constraint constraint_ var kind arg =
  {
    rc_inner = substitute_tylike_to_region constraint_.rc_inner var kind arg;
    rc_dist = substitute_tylike_to_distance constraint_.rc_dist var kind arg;
    rc_outer = substitute_tylike_to_region constraint_.rc_outer var kind arg;
  }

(** Infers the level of an ATC, checking well-formedness per the ott rules at
    lex_algeff_atm.ott:1281-1295 (ATCHole/ATCImpure/ATCFill). Type- and
    cty-wellformedness of sub-components is not verified today (matches how
    other judgments in this file skip wf passes). *)
let rec check_atc kind_env (_term_vars: (string * ty) list) (cc: atc) : distance =
  match cc with
  | ATCHole -> DZero
  | ATCVar x ->
    (match List.assoc_opt x kind_env with
    | Some (KATC d) -> d
    | Some _ -> typing_error "ATC: Variable %s is not ATC-kinded" x
    | None -> typing_error "ATC: Variable %s not found in kind environment" x)
  | ATCAns (_t, _c, cc') ->
    let l = check_atc kind_env _term_vars cc' in
    DPlus (DOne, l)
  | ATCFill (x, cc') ->
    (match List.assoc_opt x kind_env with
    | Some (KATC l1) ->
      let l2 = check_atc kind_env _term_vars cc' in
      DPlus (l1, l2)
    | Some _ -> typing_error "ATC: Variable %s in %s is not ATC-kinded" x (atc_to_str cc)
    | None -> typing_error "ATC: Variable %s in %s not found in kind environment" x (atc_to_str cc))

let rec atcs_eq cc1 cc2 =
  match cc1, cc2 with
  | ATCHole, ATCHole -> true
  | ATCVar v1, ATCVar v2 -> v1 = v2
  | ATCAns (t1, c1, rest1), ATCAns (t2, c2, rest2) ->
    types_eq t1 t2 && ctys_eq c1 c2 && atcs_eq rest1 rest2
  | ATCFill (v1, rest1), ATCFill (v2, rest2) ->
    v1 = v2 && atcs_eq rest1 rest2
  | _ -> false

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

let op_param_tys params = List.map snd params

let split_op_cty op_name (cty: cty) =
  match cty with
  | CCty (return_ty, EAns (ans_binder, c1, c2)) -> return_ty, ans_binder, c1, c2
  | CCty (_return_ty, EPure) ->
    typing_error
      "Effect operation %s: full operation signatures must return an impure cty T / C1 => C2\n"
      op_name
  | _ ->
    typing_error
      "Effect operation %s: unsupported operation cty %s\n"
      op_name (cty_to_str cty)

let rec erase_refinements_ty ty =
  match ty with
  | TRefine (_, inner, _) -> erase_refinements_ty inner
  | TRef t -> TRef (erase_refinements_ty t)
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    TFun {
      captured_set; cap_params; label_params;
      params_ty = List.map (fun (name, ty) -> (name, erase_refinements_ty ty)) params_ty;
      region;
      return_cty = erase_refinements_cty return_cty
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = erase_refinements_ty effect_return_ty;
      return_cty = erase_refinements_cty return_cty
    }
  | TNode t -> TNode (erase_refinements_ty t)
  | TTree t -> TTree (erase_refinements_ty t)
  | TQueue t -> TQueue (erase_refinements_ty t)
  | TArray t -> TArray (erase_refinements_ty t)
  | TCon (name, args) -> TCon (name, List.map erase_refinements_ty args)
  | TForall (v, k, constraints, t) -> TForall (v, k, constraints, erase_refinements_ty t)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty

and erase_refinements_cty c =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (erase_refinements_ty t, erase_refinements_eff e)
  | CFill (v, c') -> CFill (v, erase_refinements_cty c')

and erase_refinements_eff e =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (x, c1, c2) -> EAns (x, erase_refinements_cty c1, erase_refinements_cty c2)

let empty_capability = (None, Varset.empty)

let is_empty_capability cap =
  match cap with
  | None, labels -> Varset.is_empty labels
  | Some _, _ -> false

let rec replace_empty_fun_capture replacement ty =
  match ty with
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let captured_set =
      if is_empty_capability captured_set then replacement else captured_set
    in
    TFun {
      captured_set; cap_params; label_params;
      params_ty = List.map (fun (name, ty) -> (name, replace_empty_fun_capture replacement ty)) params_ty;
      region;
      return_cty = replace_empty_fun_capture_cty replacement return_cty
    }
  | TRef t -> TRef (replace_empty_fun_capture replacement t)
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = replace_empty_fun_capture replacement effect_return_ty;
      return_cty = replace_empty_fun_capture_cty replacement return_cty
    }
  | TNode t -> TNode (replace_empty_fun_capture replacement t)
  | TTree t -> TTree (replace_empty_fun_capture replacement t)
  | TQueue t -> TQueue (replace_empty_fun_capture replacement t)
  | TArray t -> TArray (replace_empty_fun_capture replacement t)
  | TCon (name, args) -> TCon (name, List.map (replace_empty_fun_capture replacement) args)
  | TForall (v, k, constraints, t) ->
    TForall (v, k, constraints, replace_empty_fun_capture replacement t)
  | TRefine (v, inner, p) -> TRefine (v, replace_empty_fun_capture replacement inner, p)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty

and replace_empty_fun_capture_cty replacement c =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (replace_empty_fun_capture replacement t, replace_empty_fun_capture_eff replacement e)
  | CFill (v, c') -> CFill (v, replace_empty_fun_capture_cty replacement c')

and replace_empty_fun_capture_eff replacement e =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (x, c1, c2) ->
    EAns (x,
          replace_empty_fun_capture_cty replacement c1,
          replace_empty_fun_capture_cty replacement c2)

(** Look up the op_cty a handler associates with the given (label, op) pair. *)
let find_op_cty label op captured_vars label_vars : op_cty =
  let lb = find_label_binding label captured_vars label_vars in
  match List.assoc_opt op lb.lb_op_ctys with
  | Some o -> o
  | None ->
    typing_error "Do: Operation %s.%s not defined by the binding handler\n" label op

(** Builds a label_binding from a handler's signature + inferred ATM (C1, C2). *)
let make_label_binding ?(op_captured_set=empty_capability) sig_name c1 c2 : label_binding =
  let op_ctys =
    match List.assoc_opt sig_name !effect_sigs_context with
    | Some ops ->
      List.map (fun (op_name, sig_) ->
        match sig_ with
        | EffectSimple (params_ty, return_ty) ->
          (op_name, {
            op_ty_bindings = [];
            op_params = List.map (fun ty -> (None, ty)) params_ty;
            op_params_ty = params_ty;
            op_return_ty = return_ty;
            op_ans_binder = None;
            op_c1 = c1;
            op_c2 = c2
          })
        | EffectFull (bindings, params, op_cty) ->
          let op_cty =
            op_cty
            |> promote_cty_vars_in_cty bindings
            |> replace_empty_fun_capture_cty op_captured_set
          in
          let return_ty, op_ans_binder, op_c1, op_c2 = split_op_cty op_name op_cty in
          (op_name, {
            op_ty_bindings = bindings;
            op_params = params;
            op_params_ty = op_param_tys params;
            op_return_ty = return_ty;
            op_ans_binder;
            op_c1;
            op_c2
          })
      ) ops
    | None -> []
  in
  { lb_effect_name = sig_name; lb_op_ctys = op_ctys }

let term_vars_with_path_hypothesis term_vars pred =
  let hyp = fresh_path_hypothesis () in
  (hyp, TRefine (hyp, TBool, pred)) :: term_vars

let branch_term_vars term_vars cond positive =
  match pred_of_expr_opt cond with
  | None -> term_vars
  | Some pred ->
    term_vars_with_path_hypothesis term_vars
      (if positive then pred else PNeg pred)

let rec syntactic_term_eq t1 t2 =
  match t1, t2 with
  | PTUnit, PTUnit -> true
  | PTInt n1, PTInt n2 -> n1 = n2
  | PTBool b1, PTBool b2 -> b1 = b2
  | PTVar x, PTVar y -> x = y
  | PTArith (l1, op1, r1), PTArith (l2, op2, r2) ->
    op1 = op2 && syntactic_term_eq l1 l2 && syntactic_term_eq r1 r2
  | _ -> false

let rec syntactic_pred_holds = function
  | PAtom (PTBool true) -> true
  | PAtom _ -> false
  | PCmp (t1, CEq, t2) -> syntactic_term_eq t1 t2
  | PCmp (t1, CNeq, t2) -> not (syntactic_term_eq t1 t2)
  | PCmp _ -> false
  | PBArith (p1, BConj, p2) ->
    syntactic_pred_holds p1 && syntactic_pred_holds p2
  | PBArith (p1, BDisj, p2) ->
    syntactic_pred_holds p1 || syntactic_pred_holds p2
  | PNeg p -> not (syntactic_pred_holds p)
  | PApp _ -> false

let rec pred_contains_app = function
  | PApp _ -> true
  | PAtom _ | PCmp _ -> false
  | PNeg p -> pred_contains_app p
  | PBArith (p1, _, p2) -> pred_contains_app p1 || pred_contains_app p2

let rec ty_contains_pred_app = function
  | TRef t
  | TNode t
  | TTree t
  | TQueue t
  | TArray t -> ty_contains_pred_app t
  | TFun { params_ty; return_cty; _ } ->
    List.exists (fun (_, t) -> ty_contains_pred_app t) params_ty
    || cty_contains_pred_app return_cty
  | TCont { effect_return_ty; return_cty; _ } ->
    ty_contains_pred_app effect_return_ty || cty_contains_pred_app return_cty
  | TCon (_, args) -> List.exists ty_contains_pred_app args
  | TForall (_, _, _, t) -> ty_contains_pred_app t
  | TRefine (_, inner, pred) ->
    ty_contains_pred_app inner || pred_contains_app pred
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> false

and cty_contains_pred_app = function
  | CTyVar _ -> false
  | CCty (t, eff) -> ty_contains_pred_app t || eff_contains_pred_app eff
  | CFill (_, c) -> cty_contains_pred_app c

and eff_contains_pred_app = function
  | EPure | EEffVar _ -> false
  | EAns (_, c1, c2) -> cty_contains_pred_app c1 || cty_contains_pred_app c2

let nullary_constructor_type con_name =
  List.find_map
    (fun (type_name, (type_params, type_cons)) ->
       match List.assoc_opt con_name type_cons with
       | Some [] -> Some (TCon (type_name, List.map (fun tv -> TVar tv) type_params))
       | _ -> None)
    !type_defs_context

(** Look up a data constructor: returns its owning type name, the type
    parameters of that type, and the declared field types of the constructor. *)
let constructor_def con_name =
  List.find_map
    (fun (type_name, (type_params, type_cons)) ->
       match List.assoc_opt con_name type_cons with
       | Some field_tys -> Some (type_name, type_params, field_tys)
       | None -> None)
    !type_defs_context

let handler_cty_var_default handle_final =
  match handle_final with
  | CCty (TFun { return_cty = CCty (_, EAns (_, _, final_cty)); _ }, EPure) ->
    final_cty
  | _ -> handle_final

(** Checks whether expression e has value type ty. Effects are not constrained.
    Raises an error with expected and actual types if the value parts don't match.

    Refinements: the actual-vs-expected comparison goes through [types_sub] so
    that [{v:T|P} <: T] (dropping a refinement) succeeds without a VC. The
    other directions ([T <: {v:T|P}] and [{v:T|Q} <: {v:T|P}]) need Z3 and
    flow through [check_refined_subtype] below. *)
let rec check_ty ?(msg = "") rctx captured_vars cap_vars label_vars term_vars (e: expr) ty =
  let te = type_expr rctx captured_vars cap_vars label_vars term_vars e in
  let actual = ty_of_cty te.expr_cty in
  if types_eq ty actual || types_sub actual ty then te
  else if refined_type_sub rctx term_vars actual ty then te
  else if check_refined_subtype rctx term_vars (Some e) actual ty then te
  else
    typing_error
      "%s\n\tExpected: %s\n\tActual: %s\n"
      msg
      (type_to_str ty)
      (type_to_str actual)

(** Refinement-aware subtype check. Two non-trivial directions handled here
    (the "drop refinement" direction is already covered by [types_sub]):

    - [actual: T <: expected: {v:T|P}]    discharge: hyps |= P[v := [[e]]]
    - [actual: {v:T|Q} <: expected: {v:T|P}]
      discharge: hyps /\ Q[v := [[e]]] |= P[v := [[e]]]

    [hyps] are gathered from refinements present in [term_vars]. The witness
    expression is converted to the predicate syntax and encoded by the SMT
    backend; if it is not encodable, it becomes a fresh symbol — sound but
    coarser.
    *)
and check_refined_subtype _rctx term_vars witness actual expected =
  match expected with
  | TRefine (ve, te, qe) ->
    let actual_base =
      match actual with
      | TRefine (_, ta, _) -> ta
      | t -> t
    in
    if not (types_eq actual_base te) then false
    else if Refinement.smt_sort_of_ty te = None then
      (match witness with
       | Some w ->
         (match pred_term_of_expr_opt w with
          | Some witness_term ->
            syntactic_pred_holds (subst_var_in_pred qe ve witness_term)
          | None -> false)
       | None -> false)
    else begin
      (* If the witness expression cannot be encoded by the SMT backend (for
         example, a function call), replace it with a fresh symbol of the
         expected base sort. This is conservative: Z3 has less information
         about the value, but the checker does not crash or assume more than
         it knows. *)
      let witness_term, extra_term_vars =
        match witness with
        | Some w ->
          (match pred_term_of_expr_opt w with
           | Some t when Refinement.pred_term_to_smt_opt t <> None -> Some t, []
           | _ ->
             let fresh = fresh_refinement_witness () in
             Some (PTVar fresh), [(fresh, te)])
        | None -> None, [(ve, te)]
      in
      let actual_extra_hyps =
        match actual with
        | TRefine (va, _, qa) ->
          let w =
            match witness_term with
            | Some w -> w
            | None -> PTVar ve
          in
          [subst_var_in_pred qa va w]
        | _ -> []
      in
      (* Form goal as P[v := witness]. If no witness, leave [v] free. *)
      let goal =
        match witness_term with
        | Some w -> subst_var_in_pred qe ve w
        | None -> qe
      in
      match Refinement.pred_to_smt_opt goal with
      | None -> false
      | Some _ ->
        if List.exists (fun h -> Refinement.pred_to_smt_opt h = None) actual_extra_hyps then false
        else
          Refinement.entails
            ~term_vars:(extra_term_vars @ term_vars)
            actual_extra_hyps goal
      end
  | _ -> false

(** Refinement-aware subtyping for computation/effect composition. The common
    structural relation stays cheap; this layer only runs in the typechecker
    where the term environment is available for SMT entailment. *)
and refined_type_sub ?(kind_env = []) rctx term_vars actual expected =
  if types_eq actual expected || types_sub ~kind_env actual expected then true
  else
    let actual = alpha_normalize actual in
    let expected = alpha_normalize expected in
    match actual, expected with
    | TFun {
        captured_set = cs1;
        cap_params = cp1;
        label_params = lp1;
        params_ty = pt1;
        region = r1;
        return_cty = rc1
      },
      TFun {
        captured_set = cs2;
        cap_params = cp2;
        label_params = lp2;
        params_ty = pt2;
        region = r2;
        return_cty = rc2
      } ->
      captured_set_sub cs1 cs2
      && cp1 = cp2
      && lp1 = lp2
      && region_requirement_sub rctx r1 r2
      && List.length pt1 = List.length pt2
      && List.for_all2
           (fun (actual_name, actual_param) (expected_name, expected_param) ->
             optional_param_names_compatible actual_name expected_name
             && refined_type_sub ~kind_env rctx term_vars expected_param actual_param)
           pt1 pt2
      && refined_cty_sub ~kind_env rctx term_vars rc1 rc2
    | TCont {
        captured_set = cs1;
        effect_return_var = erv1;
        effect_return_ty = et1;
        return_cty = rc1
      },
      TCont {
        captured_set = cs2;
        effect_return_var = erv2;
        effect_return_ty = et2;
        return_cty = rc2
      } ->
      captured_set_sub cs1 cs2
      && erv1 = erv2
      && types_eq et1 et2
      && refined_cty_sub ~kind_env rctx term_vars rc1 rc2
    | TRefine _, _
    | _, TRefine _ ->
      check_refined_subtype rctx term_vars None actual expected
    | _ -> false

and refined_eff_sub ?(kind_env = []) rctx term_vars actual expected =
  if effs_eq actual expected || eff_sub ~kind_env actual expected then true
  else
    match actual, expected with
    | EAns (x1, c11, c12), EAns (x2, c21, c22) ->
      x1 = x2
      && refined_cty_sub ~kind_env rctx term_vars c21 c11
      && refined_cty_sub ~kind_env rctx term_vars c12 c22
    | _ -> false

and refined_cty_sub ?(kind_env = []) rctx term_vars actual expected =
  if ctys_eq actual expected || cty_sub ~kind_env actual expected then true
  else
    match actual, expected with
    | CCty (t1, e1), CCty (t2, e2) ->
      refined_type_sub ~kind_env rctx term_vars t1 t2
      && refined_eff_sub ~kind_env rctx term_vars e1 e2
    | CFill (x1, c1), CFill (x2, c2) ->
      x1 = x2
      && (match List.assoc_opt x1 kind_env with
          | None | Some (KATC _) -> true
          | Some _ -> false)
      && refined_cty_sub ~kind_env rctx term_vars c1 c2
    | _ -> false

and compatible_answer_cty rctx term_vars c1 c2 =
  ctys_eq c1 c2
  || refined_cty_sub ~kind_env:rctx.kind_env rctx term_vars c1 c2
  || refined_cty_sub ~kind_env:rctx.kind_env rctx term_vars c2 c1
  || compatible_pure_function_answer rctx term_vars c1 c2
  || compatible_pure_function_final rctx term_vars c1 c2
  || compatible_pure_function_final rctx term_vars c2 c1

and compatible_pure_function_answer rctx term_vars c1 c2 =
  match c1, c2 with
  | ( CCty (TFun { return_cty = rc1; _ }, EPure),
      CCty (TFun { return_cty = rc2; _ }, EPure) ) ->
    compatible_answer_cty rctx term_vars rc1 rc2
  | _ -> false

and compatible_pure_function_final rctx term_vars inner outer =
  match outer with
  | CCty (TFun { return_cty = CCty (_, EAns (_, _, return_final)); _ }, EPure) ->
    compatible_answer_cty rctx term_vars inner return_final
  | _ -> false

and answer_final_for_return seed e =
  match seed.initial_binder with
  | Some x -> substitute_term_to_cty seed.initial_cty x e
  | None -> seed.initial_cty

and initial_answer_family_of_threaded te =
  match te.expr_cty with
  | CCty (_, EAns (binder, initial_cty, _)) -> binder, initial_cty
  | _ ->
    typing_error
      "ATC inference: expected threaded computation, got %s\n"
      (cty_to_str te.expr_cty)

and instantiate_answer_binder binder initial_cty witness =
  match binder with
  | None -> initial_cty
  | Some x -> substitute_term_to_cty initial_cty x witness

and initial_answer_for_let_continuation te x =
  let binder, initial_cty = initial_answer_family_of_threaded te in
  instantiate_answer_binder binder initial_cty (Var x)

and initial_answer_for_stmt_continuation te =
  let binder, initial_cty = initial_answer_family_of_threaded te in
  match binder with
  | None -> initial_cty
  | Some x when cty_mentions_term x initial_cty ->
    typing_error
      "Stmt: left computation's answer type depends on discarded result %s\n"
      x
  | Some _ -> initial_cty

and compatible_answer_family rctx term_vars value_ty actual_binder actual_initial expected_binder expected_initial =
  let binder, actual_initial, expected_initial =
    match actual_binder, expected_binder with
    | Some x, Some y when x <> y ->
      Some x, actual_initial, substitute_term_to_cty expected_initial y (Var x)
    | Some x, _ -> Some x, actual_initial, expected_initial
    | None, Some y -> Some y, actual_initial, expected_initial
    | None, None -> None, actual_initial, expected_initial
  in
  let term_vars =
    match binder with
    | None -> term_vars
    | Some x -> (x, value_ty) :: term_vars
  in
  compatible_answer_cty rctx term_vars actual_initial expected_initial

and answer_family_sub rctx term_vars value_ty actual_binder actual_initial expected_binder expected_initial =
  let binder, actual_initial, expected_initial =
    match actual_binder, expected_binder with
    | Some x, Some y when x <> y ->
      Some x, actual_initial, substitute_term_to_cty expected_initial y (Var x)
    | Some x, _ -> Some x, actual_initial, expected_initial
    | None, Some y -> Some y, actual_initial, expected_initial
    | None, None -> None, actual_initial, expected_initial
  in
  let term_vars =
    match binder with
    | None -> term_vars
    | Some x -> (x, value_ty) :: term_vars
  in
  refined_cty_sub ~kind_env:rctx.kind_env rctx term_vars actual_initial expected_initial
  || compatible_answer_cty rctx term_vars actual_initial expected_initial

and pred_term_mentions_term x = function
  | PTVar y -> x = y
  | PTArith (t1, _, t2) ->
    pred_term_mentions_term x t1 || pred_term_mentions_term x t2
  | PTCon (_, args) -> List.exists (pred_term_mentions_term x) args
  | PTUnit | PTInt _ | PTBool _ -> false

and pred_mentions_term x = function
  | PAtom t -> pred_term_mentions_term x t
  | PCmp (t1, _, t2) ->
    pred_term_mentions_term x t1 || pred_term_mentions_term x t2
  | PApp (_, args) ->
    List.exists (pred_term_mentions_term x) args
  | PNeg p -> pred_mentions_term x p
  | PBArith (p1, _, p2) ->
    pred_mentions_term x p1 || pred_mentions_term x p2

and ty_mentions_term x = function
  | TRef t
  | TNode t
  | TTree t
  | TQueue t
  | TArray t ->
    ty_mentions_term x t
  | TFun { params_ty; return_cty; _ } ->
    List.exists (fun (_, ty) -> ty_mentions_term x ty) params_ty
    || if List.exists (fun (param, _) -> param = Some x) params_ty
       then false
       else cty_mentions_term x return_cty
  | TCont { effect_return_var; effect_return_ty; return_cty; _ } ->
    ty_mentions_term x effect_return_ty
    || (match effect_return_var with
        | Some y when y = x -> false
        | _ -> cty_mentions_term x return_cty)
  | TCon (_, args) ->
    List.exists (ty_mentions_term x) args
  | TForall (_, _, _, body) ->
    ty_mentions_term x body
  | TRefine (binder, inner, pred) ->
    ty_mentions_term x inner
    || (binder <> x && pred_mentions_term x pred)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> false

and cty_mentions_term x = function
  | CTyVar _ -> false
  | CCty (t, eff) ->
    ty_mentions_term x t || eff_mentions_term x eff
  | CFill (_, c) ->
    cty_mentions_term x c

and eff_mentions_term x = function
  | EPure | EEffVar _ -> false
  | EAns (Some y, _, c2) when y = x ->
    cty_mentions_term x c2
  | EAns (_, c1, c2) ->
    cty_mentions_term x c1 || cty_mentions_term x c2

and peel_one_answer_layer c =
  match c with
  | CCty (t, EAns (_, c1, c2)) -> Some (t, c1, c2)
  | _ -> None

and peel_pure_function_answer_layer c =
  match c with
  | CCty ((TFun { return_cty = CCty (_, EAns (_, c1, c2)); _ } as t), EPure) ->
    Some (t, c1, c2)
  | _ -> None

and pop_one_distance distance =
  match distance_atoms (normalize_distance distance) with
  | DOne :: rest -> Some (distance_of_atoms rest)
  | _ -> None

and try_infer_atc_from_answer rctx term_vars inherited op_c1 distance =
  let rec go inherited distance =
    match normalize_distance distance with
    | DZero ->
      if compatible_answer_cty rctx term_vars inherited op_c1 then Some ATCHole
      else None
    | distance ->
      (match pop_one_distance distance with
       | None -> None
       | Some rest ->
         let continue_with_frame t frame_initial frame_final =
           Option.map
             (fun rest_atc -> ATCAns (t, frame_initial, rest_atc))
             (go frame_final rest)
         in
         match peel_one_answer_layer inherited with
         | Some (t, frame_initial, frame_final) ->
           continue_with_frame t frame_initial frame_final
         | None ->
           if distance_eq rest DZero then
             match inherited with
             | CCty (t, EPure) -> Some (ATCAns (t, op_c1, ATCHole))
             | _ -> None
           else
             match peel_pure_function_answer_layer inherited with
             | Some (t, frame_initial, frame_final) ->
               continue_with_frame t frame_initial frame_final
             | _ -> None)
  in
  go inherited distance

and lift_expr_to_answer rctx term_vars seed source_expr te =
  match te.expr_cty with
  | CCty (t, EPure) ->
    { te with expr_cty = CCty (t, EAns (seed.initial_binder, seed.initial_cty, answer_final_for_return seed source_expr)) }
  | CCty (t, EAns (_, actual_initial, actual_final)) ->
    if compatible_answer_cty rctx term_vars seed.initial_cty actual_initial then
      { te with expr_cty = CCty (t, EAns (seed.initial_binder, seed.initial_cty, actual_final)) }
    else
      typing_error
        "ATC inference: inherited initial answer does not match synthesized computation\n\tInherited: %s\n\tSynthesized: %s\n"
        (cty_to_str seed.initial_cty)
        (cty_to_str actual_initial)
  | _ ->
    typing_error
      "ATC inference: unsupported computation type %s\n"
      (cty_to_str te.expr_cty)

and compose_effs_refined ?(kind_env = []) rctx term_vars e1 e2 =
  match e1, e2 with
  | EPure, _ -> e2
  | _, EPure -> e1
  | EAns (_x1, c_in_1, c_out_1), EAns (x2, c_in_2, c_out_2) ->
    if refined_cty_sub ~kind_env rctx term_vars c_out_2 c_in_1 then
      EAns (x2, c_in_2, c_out_1)
    else
      typing_error
        "Effect composition: answer types disagree\n\tFirst needs input %s\n\tSecond produces output %s\n"
        (cty_to_str c_in_1)
        (cty_to_str c_out_2)
  | _ ->
    typing_error
      "Effect composition: cannot compose %s with %s\n"
      (eff_to_str e1)
      (eff_to_str e2)

(** Validates refinement predicates structurally. The SMT backend emits
    [set-logic ALL], so nonlinear integer terms are left to Z3 instead of
    rejected here. *)
and validate_pred (p: SLsyntax.pred) =
  let rec go_term t =
    match t with
    | PTUnit | PTInt _ | PTBool _ | PTVar _ -> ()
    | PTCon (_, args) -> List.iter go_term args
    | PTArith (t1, _, t2) -> go_term t1; go_term t2
  in
  let rec go p =
    match p with
    | PAtom t -> go_term t
    | PCmp (t1, _, t2) -> go_term t1; go_term t2
    | PApp (_, args) -> List.iter go_term args
    | PBArith (p1, _, p2) -> go p1; go p2
    | PNeg p' -> go p'
  in go p

and ty_of_base_ty = function
  | BInt -> TInt
  | BBool -> TBool
  | BUnit -> TUnit

and base_ty_of_ty = function
  | TInt -> Some BInt
  | TBool -> Some BBool
  | TUnit -> Some BUnit
  | _ -> None

and strip_outer_refinement = function
  | TRefine (_, inner, _) -> inner
  | t -> t

and check_pred_expected rctx term_vars p expected =
  let actual = pred_type rctx term_vars p in
  if not (types_eq actual expected || types_sub actual expected) then
    typing_error
      "Refinement predicate: expected %s but got %s in %s\n"
      (type_to_str expected) (type_to_str actual) (pred_to_str p)

and check_pred_term_expected rctx term_vars t expected =
  let actual = pred_term_type rctx term_vars t in
  if not (types_eq actual expected || types_sub actual expected) then
    typing_error
      "Refinement predicate term: expected %s but got %s in %s\n"
      (type_to_str expected) (type_to_str actual) (pred_term_to_str t)

and pred_term_type rctx term_vars (t: pred_term) : ty =
  match t with
  | PTUnit -> TUnit
  | PTInt _ -> TInt
  | PTBool _ -> TBool
  | PTVar x ->
    (match List.assoc_opt x term_vars with
     | Some t -> t
     | None ->
       (match nullary_constructor_type x with
        | Some t -> t
        | None -> typing_error "Refinement predicate: variable %s not found\n" x))
  | PTArith (t1, _, t2) ->
    check_pred_term_expected rctx term_vars t1 TInt;
    check_pred_term_expected rctx term_vars t2 TInt;
    TInt
  | PTCon (con, args) ->
    (match constructor_def con with
     | None -> typing_error "Refinement predicate: constructor %s not found\n" con
     | Some (type_name, type_params, field_tys) ->
       if List.length args <> List.length field_tys then
         typing_error
           "Refinement predicate: constructor %s expects %d argument(s), got %d\n"
           con (List.length field_tys) (List.length args);
       if type_params = [] then begin
         List.iter2
           (fun a fty -> check_pred_term_expected rctx term_vars a (strip_outer_refinement fty))
           args field_tys;
         TCon (type_name, [])
       end else
         (* Parametric constructors: keep the type-variable shape; argument
            checking against instantiated fields is left to the value-level
            typechecker. *)
         TCon (type_name, List.map (fun tv -> TVar tv) type_params))

and pred_type rctx term_vars (p: pred) : ty =
  match p with
  | PAtom t -> pred_term_type rctx term_vars t
  | PCmp (t1, op, t2) ->
    if op = CEq || op = CNeq then begin
      let ty1 = strip_outer_refinement (pred_term_type rctx term_vars t1) in
      let ty2 = strip_outer_refinement (pred_term_type rctx term_vars t2) in
      if types_eq ty1 ty2 then TBool
      else
        typing_error
          "Refinement predicate: equality operands have different types in %s\n\tLeft: %s\n\tRight: %s\n"
          (pred_to_str p) (type_to_str ty1) (type_to_str ty2)
    end else begin
      check_pred_term_expected rctx term_vars t1 TInt;
      check_pred_term_expected rctx term_vars t2 TInt;
      TBool
    end
  | PBArith (p1, _, p2) ->
    check_pred_expected rctx term_vars p1 TBool;
    check_pred_expected rctx term_vars p2 TBool;
    TBool
  | PNeg p' ->
    check_pred_expected rctx term_vars p' TBool;
    TBool
  | PApp (pred_name, args) ->
    let expected_args =
      match List.assoc_opt pred_name rctx.kind_env with
      | Some (KPred args) -> args
      | Some _ -> typing_error "Predicate application: %s is not predicate-kinded\n" pred_name
      | None -> typing_error "Predicate application: predicate variable %s not found\n" pred_name
    in
    if List.length expected_args <> List.length args then
      typing_error
        "Predicate application: %s expected %d arguments but got %d\n"
        pred_name (List.length expected_args) (List.length args);
    List.iter2
      (fun arg bty -> check_pred_term_expected rctx term_vars arg (ty_of_base_ty bty))
      args expected_args;
    TBool

and check_pred_tylike rctx term_vars expected_args params body =
  if List.length expected_args <> List.length params then
    typing_error
      "Predicate instantiation: expected %d parameters but got %d\n"
      (List.length expected_args) (List.length params);
  List.iter2
    (fun (_name, ty) expected ->
      match base_ty_of_ty ty with
      | Some actual when actual = expected -> ()
      | _ ->
        typing_error
          "Predicate instantiation: expected parameter type %s but got %s\n"
          (type_to_str (ty_of_base_ty expected)) (type_to_str ty))
    params expected_args;
  validate_pred body;
  check_pred_expected rctx (params @ term_vars) body TBool

(** Walks a type and, for every [TRefine(v, base, p)] encountered, validates
    [p] structurally and type-checks it as [TBool] under the env extended
    with [(v, base)]. *)
and check_refinement_wellformed rctx term_vars (ty: ty) =
  let rec go term_vars ty = match ty with
    | TRefine (v, inner, p) ->
      validate_pred p;
      check_pred_expected rctx ((v, inner) :: term_vars) p TBool;
      go term_vars inner
    | TRef inner -> go term_vars inner
    | TFun { params_ty; return_cty; _ } ->
      let term_vars' =
        List.fold_left
          (fun acc (name, param_ty) ->
            go acc param_ty;
            match name with
            | Some x -> (x, param_ty) :: acc
            | None -> acc)
          term_vars params_ty
      in
      go_cty term_vars' return_cty
    | TCont { effect_return_var; effect_return_ty; return_cty; _ } ->
      go term_vars effect_return_ty;
      let term_vars' =
        match effect_return_var with
        | Some x -> (x, effect_return_ty) :: term_vars
        | None -> term_vars
      in
      go_cty term_vars' return_cty
    | TNode t | TTree t | TQueue t | TArray t ->
      go term_vars t
    | TCon (_, t_args) ->
      List.iter (go term_vars) t_args
    | TForall (_, _, _, t) -> go term_vars t
    | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ()
  and go_cty term_vars c = match c with
    | CCty (t, EAns (Some x, c1, c2)) ->
      go term_vars t;
      go_cty ((x, t) :: term_vars) c1;
      go_cty term_vars c2
    | CCty (t, eff) ->
      go term_vars t;
      go_eff term_vars eff
    | CTyVar _ -> ()
    | CFill (_, c') -> go_cty term_vars c'
  and go_eff term_vars eff =
    match eff with
    | EPure | EEffVar _ -> ()
    | EAns (None, c1, c2) ->
      go_cty term_vars c1;
      go_cty term_vars c2
    | EAns (Some _, c1, c2) ->
      (* Answer-effect binders are checked with their value type in [go_cty],
         where the enclosing [T / (x.C1)=>C2] exposes that type. *)
      go_cty term_vars c1;
      go_cty term_vars c2
  in go term_vars ty

and type_raise_expr ?final_answer rctx captured_vars cap_vars label_vars term_vars
    raise_label raise_op raise_tylikes raise_atc_opt raise_args =
  let op_cty_info = find_op_cty raise_label raise_op captured_vars label_vars in
  if List.length raise_tylikes <> List.length op_cty_info.op_ty_bindings then
    typing_error
      "Raise: Incorrect number of type/predicate/cty instantiations for %s.%s\n\tExpected: %d\n\tActual: %d\n"
      raise_label raise_op
      (List.length op_cty_info.op_ty_bindings)
      (List.length raise_tylikes);
  List.iter2 (fun (_name, kind) arg ->
    match kind, arg with
    | KTy, TLTy _ -> ()
    | KCty, TLCty _ -> ()
    | KCty, TLTy _ -> ()
    | KEff, TLEff _ -> ()
    | KDist, TLDist _ -> ()
    | KATC expected_dist, TLATC cc ->
      let actual_dist =
        check_atc (op_cty_info.op_ty_bindings @ rctx.kind_env) term_vars cc
      in
      if not (distance_eq actual_dist expected_dist) then
        typing_error
          "Raise: ATC instantiation has wrong distance for %s.%s\n"
          raise_label raise_op
    | KReg, _ when region_of_tylike arg <> None -> ()
    | KPred expected_args, TLPred (params, body) ->
      check_pred_tylike rctx term_vars expected_args params body
    | _ ->
      typing_error
        "Raise: Type-like instantiation has wrong kind for %s.%s\n"
        raise_label raise_op
  ) op_cty_info.op_ty_bindings raise_tylikes;
  let instantiate_type ty =
    substitute_tylikes_to_type ty op_cty_info.op_ty_bindings raise_tylikes
  in
  let instantiate_cty c =
    substitute_tylikes_to_cty c op_cty_info.op_ty_bindings raise_tylikes
  in
  let op_params_ty = List.map instantiate_type op_cty_info.op_params_ty in
  let op_return_ty = instantiate_type op_cty_info.op_return_ty in
  let op_c1 = instantiate_cty op_cty_info.op_c1 in
  let op_c2 = instantiate_cty op_cty_info.op_c2 in
  let choose_atc_and_constraints op_c2 =
    match final_answer, List.assoc_opt raise_label rctx.label_regions, raise_atc_opt with
    | Some final_cty, Some target_region, None ->
      (match resolve_subregion_or_flex rctx rctx.current_region target_region [] with
       | Some (Complete distance) ->
         (match try_infer_atc_from_answer rctx term_vars final_cty op_c2 distance with
          | Some inferred_atc -> inferred_atc, []
          | None ->
            if distance_eq distance DZero then ATCHole, []
            else
              typing_error
                "Raise: could not infer ATC for %s.%s from final answer\n\tFinal answer: %s\n\tOperation output answer: %s\n\tDistance: %s\n"
                raise_label raise_op
                (cty_to_str final_cty)
                (cty_to_str op_c2)
                (distance_to_str distance))
       | Some (Blocked _) ->
         typing_error
           "Raise: cannot infer ATC for %s.%s before the subregion path is resolved\n"
           raise_label raise_op
       | None ->
         typing_error
           "Raise: cannot infer ATC for %s.%s without a subregion path\n"
           raise_label raise_op)
    | Some final_cty, Some target_region, Some raise_atc ->
      (match resolve_subregion_or_flex rctx rctx.current_region target_region [] with
       | Some (Complete distance) ->
         (match try_infer_atc_from_answer rctx term_vars final_cty op_c2 distance with
          | Some inferred_atc ->
            if not (atcs_eq inferred_atc raise_atc) then
              typing_error
                "Raise: supplied ATC does not match the final-answer-inferred context for %s.%s\n\tSupplied: %s\n\tInferred: %s\n"
                raise_label raise_op
                (atc_to_str raise_atc)
                (atc_to_str inferred_atc);
            inferred_atc, []
          | None ->
            let l_atc = check_atc rctx.kind_env term_vars raise_atc in
            raise_atc, check_subregion rctx rctx.current_region target_region l_atc)
       | _ ->
         let l_atc = check_atc rctx.kind_env term_vars raise_atc in
         raise_atc, check_subregion rctx rctx.current_region target_region l_atc)
    | _, Some target_region, Some raise_atc ->
      let l_atc = check_atc rctx.kind_env term_vars raise_atc in
      raise_atc, check_subregion rctx rctx.current_region target_region l_atc
    | _, Some target_region, None ->
      (match resolve_subregion_or_flex rctx rctx.current_region target_region [] with
       | Some (Complete DZero) -> ATCHole, []
       | _ ->
         typing_error
           "Raise: omitted ATC for %s.%s requires an expected answer context\n"
           raise_label raise_op)
    | _, None, Some raise_atc ->
      let _ = check_atc rctx.kind_env term_vars raise_atc in
      raise_atc, []
    | _, None, None ->
      ATCHole, []
  in
  if (List.length raise_args) != (List.length op_params_ty) then
    typing_error
      "Raise: Incorrect number of arguments\n\tExpected: %d\n\tActual: %d\n"
      (List.length op_params_ty)
      (List.length raise_args)
  else
    let raise_args' =
      List.map2
        (fun arg ty ->
           check_ty ~msg:"Raise: Parameter types don't match"
             rctx captured_vars cap_vars label_vars term_vars arg ty)
        raise_args op_params_ty
    in
    let op_return_ty, op_c1, op_c2 =
      List.fold_left2
        (fun (return_ty, c1, c2) (param_name, _) arg ->
           match param_name with
           | None -> return_ty, c1, c2
           | Some x ->
             ( substitute_term_to_type return_ty x arg,
               substitute_term_to_cty c1 x arg,
               substitute_term_to_cty c2 x arg ))
        (op_return_ty, op_c1, op_c2)
        op_cty_info.op_params raise_args
    in
    let raise_atc, subregion_constraints = choose_atc_and_constraints op_c2 in
    let kind_env = rctx.kind_env in
    let args_eff =
      List.fold_left
        (fun acc te -> compose_effs_refined ~kind_env rctx term_vars acc (eff_of_cty te.expr_cty))
        EPure raise_args'
    in
    let c1' = fill_atc raise_atc op_c1 in
    let c2' = fill_atc raise_atc op_c2 in
    let raise_eff = EAns (op_cty_info.op_ans_binder, c1', c2') in
    let eff = compose_effs_refined ~kind_env rctx term_vars args_eff raise_eff in
    ( Raise {
        raise_label;
        raise_op;
        raise_tylikes;
        raise_atc;
        raise_args = raise_args'
      },
      CCty (op_return_ty, eff),
      subregion_constraints )

(** Checks an expression against an expected computation type. For impure
    expectations, the expected final answer is threaded forward through the
    expression so Raise sites recover their ATC by factoring the final answer
    against the operation's C2. Pure tail expressions still use the expected
    initial answer as the return-answer family. *)
and check_cty ?(msg = "") rctx captured_vars cap_vars label_vars term_vars (e: expr) expected =
  match e with
  (* Push the full expected computation type (value refinement *and* answer)
     into both branches of a conditional, under the branch path conditions.
     This lets a refined value (e.g. a data constructor [Some(n/m)]) be checked
     leaf-by-leaf against the expected refinement even when the handler's answer
     type is impure -- the branch leaf becomes the witness for the SMT check. *)
  | If (cond, e1, e2) ->
    let cond' =
      check_ty rctx captured_vars cap_vars label_vars term_vars cond TBool
        ~msg:"If: Expected bool as condition"
    in
    let then_vars = branch_term_vars term_vars cond true in
    let else_vars = branch_term_vars term_vars cond false in
    let e1' = check_cty ~msg rctx captured_vars cap_vars label_vars then_vars e1 expected in
    let e2' = check_cty ~msg rctx captured_vars cap_vars label_vars else_vars e2 expected in
    let eff =
      compose_effs_refined ~kind_env:rctx.kind_env rctx term_vars
        (eff_of_cty cond'.expr_cty) (eff_of_cty expected)
    in
    let actual = CCty (ty_of_cty expected, eff) in
    if refined_cty_sub rctx term_vars actual expected then
      {
        expr_desc = If (cond', e1', e2');
        expr_cty = expected;
        region_constraints =
          constraints_unions
            [cond'.region_constraints; e1'.region_constraints; e2'.region_constraints];
        captured_vars;
        cap_vars;
        label_vars;
      }
    else
      typing_error "%s\n\tExpected: %s\n\tActual: %s\n" msg
        (cty_to_str expected) (cty_to_str actual)
  (* Checking-mode for the value-binding / matching forms: thread the effectful
     prefix's answer and *check* the tail / branches against the expected
     computation type, rather than synthesising and comparing.  This lets a
     refined data value flow through a handler's control flow (e.g. proving the
     empty-list scan in `select_from` yields None, so amb returns Failure). *)
  | Match { match_expr; pattern_matching } ->
    let match_expr' = type_expr rctx captured_vars cap_vars label_vars term_vars match_expr in
    let match_expr_ty = strip_outer_refinement (ty_of_cty match_expr'.expr_cty) in
    (match match_expr_ty with
     | TCon (t, t_args) ->
       let type_params, type_cons = List.assoc t !type_defs_context in
       let type_subs = List.map2 (fun tv ta -> (tv, ta)) type_params t_args in
       let scrut_term = pred_term_of_expr_opt match_expr in
       let branch_hyp_vars con args =
         match scrut_term with
         | Some s ->
           [ (fresh_refinement_witness (),
              TRefine ("__match", TBool,
                       PCmp (s, CEq, PTCon (con, List.map (fun a -> PTVar a) args)))) ]
         | None -> []
       in
       let branches' =
         List.map
           (fun (pt, res) ->
              match pt with
              | Syntax__Common.PTypecon (con, args) ->
                let params_type =
                  List.map (fun ty -> substitute_ty ty type_subs) (List.assoc con type_cons)
                in
                let arg_vars = List.map2 (fun a ty -> (a, ty)) args params_type in
                let bvars = arg_vars @ branch_hyp_vars con args @ term_vars in
                let res' = check_cty ~msg rctx captured_vars cap_vars label_vars bvars res expected in
                (pt, res'))
           pattern_matching
       in
       let eff =
         compose_effs_refined ~kind_env:rctx.kind_env rctx term_vars
           (eff_of_cty match_expr'.expr_cty) (eff_of_cty expected)
       in
       {
         expr_desc = Match { match_expr = match_expr'; pattern_matching = branches' };
         expr_cty = CCty (ty_of_cty expected, eff);
         region_constraints =
           constraints_unions
             (match_expr'.region_constraints
              :: List.map (fun (_, r) -> r.region_constraints) branches');
         captured_vars; cap_vars; label_vars;
       }
     | _ -> typing_error "Match: Pattern type expected, got %s instead\n" (type_to_str match_expr_ty))
  | Let (x, e1, e2) when (match expected with CCty (_, EAns _) -> true | _ -> false) ->
    let expected_ty, initial_binder, initial_cty, final_e =
      match expected with
      | CCty (expected_ty, EAns (initial_binder, initial_cty, c2)) ->
        expected_ty, initial_binder, initial_cty, c2
      | _ -> typing_error "Let: impure expected cty required"
    in
    let e1' =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e1 final_e None
    in
    let link_answer = initial_answer_for_let_continuation e1' x in
    let t1 = ty_of_cty e1'.expr_cty in
    let e2_expected = CCty (expected_ty, EAns (initial_binder, initial_cty, link_answer)) in
    let e2' =
      check_cty ~msg rctx captured_vars cap_vars label_vars
        ((x, t1) :: term_vars) e2 e2_expected
    in
    let eff =
      compose_effs_refined ~kind_env:rctx.kind_env rctx term_vars
        (eff_of_cty e1'.expr_cty) (eff_of_cty e2'.expr_cty)
    in
    {
      expr_desc = Let (x, e1', e2');
      expr_cty = CCty (ty_of_cty expected, eff);
      region_constraints = constraints_unions [e1'.region_constraints; e2'.region_constraints];
      captured_vars; cap_vars; label_vars;
    }
  | Stmt (e1, e2) when (match expected with CCty (_, EAns _) -> true | _ -> false) ->
    let expected_ty, initial_binder, initial_cty, final_e =
      match expected with
      | CCty (expected_ty, EAns (initial_binder, initial_cty, c2)) ->
        expected_ty, initial_binder, initial_cty, c2
      | _ -> typing_error "Stmt: impure expected cty required"
    in
    let e1' =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e1 final_e None
    in
    let link_answer = initial_answer_for_stmt_continuation e1' in
    let e2_expected = CCty (expected_ty, EAns (initial_binder, initial_cty, link_answer)) in
    let e2' =
      check_cty ~msg rctx captured_vars cap_vars label_vars term_vars e2 e2_expected
    in
    let eff =
      compose_effs_refined ~kind_env:rctx.kind_env rctx term_vars
        (eff_of_cty e1'.expr_cty) (eff_of_cty e2'.expr_cty)
    in
    {
      expr_desc = Stmt (e1', e2');
      expr_cty = CCty (ty_of_cty expected, eff);
      region_constraints = constraints_unions [e1'.region_constraints; e2'.region_constraints];
      captured_vars; cap_vars; label_vars;
    }
  | _ ->
  match expected with
  | CCty (expected_ty, EAns (initial_binder, initial_cty, expected_final)) ->
    let seed = { initial_binder; initial_cty } in
    let te =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e expected_final (Some seed)
    in
    let actual_ty = ty_of_cty te.expr_cty in
    let actual_initial_binder, actual_initial =
      initial_answer_family_of_threaded te
    in
    let value_ok =
      types_eq actual_ty expected_ty
      || refined_type_sub ~kind_env:rctx.kind_env rctx term_vars actual_ty expected_ty
      || check_refined_subtype rctx term_vars (Some e) actual_ty expected_ty
    in
    let initial_ok =
      compatible_answer_family rctx term_vars actual_ty
        actual_initial_binder actual_initial initial_binder initial_cty
    in
    if value_ok && initial_ok then { te with expr_cty = expected }
    else
      typing_error
        "%s\n\tExpected: %s\n\tActual: %s\n"
        msg
        (cty_to_str expected)
        (cty_to_str te.expr_cty)
  | _ ->
    (match expected, e with
     | CCty (_, EPure), If (cond, e1, e2) ->
       let cond' =
         check_ty rctx captured_vars cap_vars label_vars term_vars cond TBool
           ~msg:"If: Expected bool as condition"
       in
       let then_vars = branch_term_vars term_vars cond true in
       let else_vars = branch_term_vars term_vars cond false in
       let e1' =
         check_cty ~msg rctx captured_vars cap_vars label_vars then_vars e1 expected
       in
       let e2' =
         check_cty ~msg rctx captured_vars cap_vars label_vars else_vars e2 expected
       in
       let actual = CCty (ty_of_cty expected, eff_of_cty cond'.expr_cty) in
       if refined_cty_sub rctx term_vars actual expected then
         {
           expr_desc = If (cond', e1', e2');
           expr_cty = expected;
           region_constraints =
             constraints_unions
               [cond'.region_constraints; e1'.region_constraints; e2'.region_constraints];
           captured_vars;
           cap_vars;
           label_vars;
         }
       else
         typing_error
           "%s\n\tExpected: %s\n\tActual: %s\n"
           msg
           (cty_to_str expected)
           (cty_to_str actual)
     | _ ->
    let te = type_expr rctx captured_vars cap_vars label_vars term_vars e in
    let actual = te.expr_cty in
    if ctys_eq actual expected then te
    else if refined_cty_sub rctx term_vars actual expected then { te with expr_cty = expected }
    else begin
      (* Try a refinement-aware comparison on the value-type component. The
         effect parts must still match via [eff_sub]. *)
      let refinement_ok =
        match actual, expected with
        | CCty (ta, ea), CCty (te_, ee) ->
          eff_sub ea ee && check_refined_subtype rctx term_vars (Some e) ta te_
        | _ -> false
      in
      if refinement_ok then { te with expr_cty = expected }
      else
        typing_error
          "%s\n\tExpected: %s\n\tActual: %s\n"
          msg
          (cty_to_str expected)
          (cty_to_str actual)
    end)

and type_expr_with_final rctx (captured_vars: capture_set) cap_vars label_vars
    (term_vars: (string * ty) list) (e: expr) (final_cty: cty)
    (return_seed: answer_context option): typed_expr =
  let type_expr_plain = type_expr rctx in
  let check_ty ?(msg="") = check_ty ~msg rctx in
  let compose_effs ?(kind_env = []) e1 e2 =
    compose_effs_refined ~kind_env rctx term_vars e1 e2
  in
  let ty_of te = ty_of_cty te.expr_cty in
  let eff_of te = eff_of_cty te.expr_cty in
  let constraints_of_expr te = te.region_constraints in
  let constraints_of_exprs exprs =
    constraints_unions (List.map constraints_of_expr exprs)
  in
  let constraints_of_return_clause = function
    | None -> []
    | Some ({ return_body; _ } : Typed_ast.typed_return_clause) ->
      return_body.region_constraints
  in
  let constraints_of_hdl ({ op_body; _ } : Typed_ast.hdl) =
    op_body.region_constraints
  in
  let constraints_of_fundef ({ body; _ } : Typed_ast.fundef) =
    body.region_constraints
  in
  let constraints_of_desc = function
    | Unit | Var _ | Int _ | Float _ | Bool _ | Str _ | Char _ | Prim _ -> []
    | Arith (e1, _, e2)
    | Cmp (e1, _, e2)
    | BArith (e1, _, e2)
    | Get (e1, e2)
    | Resume (e1, e2)
    | ResumeFinal (e1, e2)
    | Stmt (e1, e2) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2]
    | Neg e -> constraints_of_expr e
    | App { func; args; _ } ->
      constraints_unions (constraints_of_expr func :: List.map constraints_of_expr args)
    | New exprs -> constraints_of_exprs exprs
    | Set (e1, e2, e3)
    | If (e1, e2, e3) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2; constraints_of_expr e3]
    | Raise { raise_args; _ } -> constraints_of_exprs raise_args
    | Handle { handle_body; return_clause; handler_defs; _ } ->
      constraints_unions [
        handle_body.region_constraints;
        constraints_of_return_clause return_clause;
        constraints_unions (List.map constraints_of_hdl handler_defs)
      ]
    | Recdef (fundefs, body) ->
      constraints_unions
        (body.region_constraints :: List.map constraints_of_fundef fundefs)
    | Fun { body; _ } -> body.region_constraints
    | Let (_, e1, e2) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2]
    | Typecon (_, _, args) -> constraints_of_exprs args
    | Match { match_expr; pattern_matching } ->
      constraints_unions
        (match_expr.region_constraints
         :: List.map (fun (_, e) -> e.region_constraints) pattern_matching)
    | TypeApp (_, e) -> constraints_of_expr e
  in
  let mk expr_desc expr_cty extra_region_constraints =
    {
      expr_desc;
      expr_cty;
      region_constraints =
        constraints_union extra_region_constraints (constraints_of_desc expr_desc);
      captured_vars;
      cap_vars;
      label_vars;
    }
  in
  let with_final ty binder initial_cty =
    CCty (ty, EAns (binder, initial_cty, final_cty))
  in
  let lift_plain source_expr te =
    match te.expr_cty with
    | CCty (t, EPure) ->
      (match return_seed with
       | Some seed ->
         let return_final = answer_final_for_return seed source_expr in
         if compatible_answer_cty rctx term_vars return_final final_cty then
           { te with expr_cty = with_final t seed.initial_binder seed.initial_cty }
         else
           { te with expr_cty = with_final t None final_cty }
       | None ->
         { te with expr_cty = with_final t None final_cty })
    | CCty (t, EAns (binder, actual_initial, actual_final)) ->
      if compatible_answer_cty rctx term_vars actual_final final_cty then
        { te with expr_cty = with_final t binder actual_initial }
      else
        typing_error
          "ATC inference: inherited final answer does not match synthesized computation\n\tInherited final: %s\n\tSynthesized final: %s\n"
          (cty_to_str final_cty)
          (cty_to_str actual_final)
    | _ ->
      typing_error
        "ATC inference: unsupported computation type %s\n"
        (cty_to_str te.expr_cty)
  in
  match e with
  | Let (x, e1, e2) ->
    let e1' =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e1 final_cty None
    in
    let link_answer = initial_answer_for_let_continuation e1' x in
    let t1 = ty_of e1' in
    let e2' =
      type_expr_with_final rctx captured_vars cap_vars label_vars
        ((x, t1) :: term_vars) e2 link_answer return_seed
    in
    let initial_binder, initial_answer = initial_answer_family_of_threaded e2' in
    mk (Let (x, e1', e2')) (with_final (ty_of e2') initial_binder initial_answer) []

  | Stmt (e1, e2) ->
    let e1' =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e1 final_cty None
    in
    let link_answer = initial_answer_for_stmt_continuation e1' in
    let e2' =
      type_expr_with_final rctx captured_vars cap_vars label_vars term_vars
        e2 link_answer return_seed
    in
    let initial_binder, initial_answer = initial_answer_family_of_threaded e2' in
    mk (Stmt (e1', e2')) (with_final (ty_of e2') initial_binder initial_answer) []

  | If (cond, e1, e2) ->
    let cond' =
      check_ty captured_vars cap_vars label_vars term_vars cond TBool
        ~msg:"If: Expected bool as condition"
    in
    let then_vars = branch_term_vars term_vars cond true in
    let else_vars = branch_term_vars term_vars cond false in
    let e1' =
      type_expr_with_final rctx captured_vars cap_vars label_vars then_vars
        e1 final_cty return_seed
    in
    let e2' =
      type_expr_with_final rctx captured_vars cap_vars label_vars else_vars
        e2 final_cty return_seed
    in
    let t1 = ty_of e1' in
    let t2 = ty_of e2' in
    if not (types_eq t1 t2) then
      typing_error
        "If: The two branches don't agree\n\tExpected: %s\n\tActual: %s\n"
        (type_to_str t1)
        (type_to_str t2);
    let initial_binder1, initial1 = initial_answer_family_of_threaded e1' in
    let initial_binder2, initial2 = initial_answer_family_of_threaded e2' in
    let initial_binder =
      match initial_binder1 with
      | Some _ -> initial_binder1
      | None -> initial_binder2
    in
    let normalize_initial branch_binder initial =
      match initial_binder, branch_binder with
      | Some x, Some y when x <> y -> substitute_term_to_cty initial y (Var x)
      | _ -> initial
    in
    let initial1 = normalize_initial initial_binder1 initial1 in
    let initial2 = normalize_initial initial_binder2 initial2 in
    let branch_term_vars =
      match initial_binder with
      | None -> term_vars
      | Some x -> (x, t1) :: term_vars
    in
    let initial_answer =
      if ctys_eq initial1 initial2
         || refined_cty_sub ~kind_env:rctx.kind_env rctx branch_term_vars initial1 initial2
      then initial2
      else if refined_cty_sub ~kind_env:rctx.kind_env rctx branch_term_vars initial2 initial1
      then initial1
      else
        typing_error
          "If: The two branches don't agree on initial answer type\n\tLeft: %s\n\tRight: %s\n"
          (cty_to_str initial1)
          (cty_to_str initial2)
    in
    let branch_eff = EAns (initial_binder, initial_answer, final_cty) in
    let eff = compose_effs (eff_of cond') branch_eff in
    mk (If (cond', { e1' with expr_cty = CCty (t1, branch_eff) },
             { e2' with expr_cty = CCty (t1, branch_eff) }))
      (CCty (t1, eff)) []

  | Match { match_expr; pattern_matching } ->
    (* Checking-mode match: thread the expected final answer into every branch
       body so that effect operations performed inside a branch (e.g. a [select]
       inside [select_from]'s [Cons] case) recover their ATC, exactly as the
       If case does.  Each branch also learns [scrutinee == C(args)] so the
       scrutinee's refinement propagates onto the payload variables. *)
    let match_expr' = type_expr rctx captured_vars cap_vars label_vars term_vars match_expr in
    let match_expr_ty = strip_outer_refinement (ty_of match_expr') in
    (match match_expr_ty with
     | TCon (t, t_args) ->
       let type_params, type_cons = List.assoc t !type_defs_context in
       let type_subs = List.map2 (fun tv ta -> (tv, ta)) type_params t_args in
       let scrut_term = pred_term_of_expr_opt match_expr in
       let branch_hyp_vars con args =
         match scrut_term with
         | Some s ->
           [ (fresh_refinement_witness (),
              TRefine ("__match", TBool,
                       PCmp (s, CEq, PTCon (con, List.map (fun a -> PTVar a) args)))) ]
         | None -> []
       in
       let branches' =
         List.map
           (fun (pt, res) ->
              match pt with
              | Syntax__Common.PTypecon (con, args) ->
                let params_type =
                  List.map (fun ty -> substitute_ty ty type_subs) (List.assoc con type_cons)
                in
                let arg_vars = List.map2 (fun a ty -> (a, ty)) args params_type in
                let bvars = arg_vars @ branch_hyp_vars con args @ term_vars in
                let res' =
                  type_expr_with_final rctx captured_vars cap_vars label_vars bvars
                    res final_cty return_seed
                in
                (pt, res'))
           pattern_matching
       in
       let value_ty =
         match branches' with
         | (_, r) :: _ -> ty_of r
         | [] -> typing_error "Match: no branches\n"
       in
       List.iter
         (fun (_, r) ->
            if not (types_eq (ty_of r) value_ty) then
              typing_error "Match: The types of clauses don't match\n\tExpected: %s\n\tActual: %s\n"
                (type_to_str value_ty) (type_to_str (ty_of r)))
         branches';
       (* Combine the per-branch initial answer families into one. *)
       let combine (b1, i1) (b2, i2) =
         let binder = match b1 with Some _ -> b1 | None -> b2 in
         let norm bb i =
           match binder, bb with
           | Some x, Some y when x <> y -> substitute_term_to_cty i y (Var x)
           | _ -> i
         in
         let i1 = norm b1 i1 and i2 = norm b2 i2 in
         let btv = match binder with None -> term_vars | Some x -> (x, value_ty) :: term_vars in
         let i =
           if ctys_eq i1 i2
              || refined_cty_sub ~kind_env:rctx.kind_env rctx btv i1 i2 then i2
           else if refined_cty_sub ~kind_env:rctx.kind_env rctx btv i2 i1 then i1
           else
             typing_error
               "Match: branches don't agree on initial answer type\n\tLeft: %s\n\tRight: %s\n"
               (cty_to_str i1) (cty_to_str i2)
         in
         (binder, i)
       in
       let initials = List.map (fun (_, r) -> initial_answer_family_of_threaded r) branches' in
       let initial_binder, initial_answer =
         match initials with
         | first :: rest -> List.fold_left combine first rest
         | [] -> (None, final_cty)
       in
       let branch_eff = EAns (initial_binder, initial_answer, final_cty) in
       let eff = compose_effs (eff_of match_expr') branch_eff in
       let branches'' =
         List.map (fun (pt, r) -> (pt, { r with expr_cty = CCty (value_ty, branch_eff) })) branches'
       in
       mk (Match { match_expr = match_expr'; pattern_matching = branches'' })
         (CCty (value_ty, eff)) []
     | _ -> typing_error "Match: Pattern type expected, got %s instead\n" (type_to_str match_expr_ty))

  | Raise { raise_label; raise_op; raise_tylikes; raise_atc; raise_args } ->
    let expr_desc, expr_cty, region_constraints =
      type_raise_expr ~final_answer:final_cty rctx captured_vars cap_vars label_vars
        term_vars raise_label raise_op raise_tylikes raise_atc raise_args
    in
    let expr_cty =
      match expr_cty with
      | CCty (t, EAns (binder, initial_cty, _actual_final)) ->
        CCty (t, EAns (binder, initial_cty, final_cty))
      | _ -> expr_cty
    in
    mk expr_desc expr_cty region_constraints

  | _ ->
    let te = type_expr_plain captured_vars cap_vars label_vars term_vars e in
    lift_plain e te

and type_expr rctx (captured_vars: capture_set) cap_vars label_vars (term_vars: (string * ty) list) (e: expr): typed_expr =
  let type_expr_with = type_expr in
  let check_cty_with = check_cty in
  let type_expr = type_expr rctx in
  let check_ty ?(msg="") = check_ty ~msg rctx in
  let compose_effs ?(kind_env = []) e1 e2 =
    compose_effs_refined ~kind_env rctx term_vars e1 e2
  in
  let ty_of te = ty_of_cty te.expr_cty in
  let eff_of te = eff_of_cty te.expr_cty in
  let lift_to_cty ?(msg = "") te expected =
    if ctys_eq te.expr_cty expected then te
    else if refined_cty_sub rctx term_vars te.expr_cty expected then { te with expr_cty = expected }
    else
      typing_error
        "%s\n\tExpected: %s\n\tActual: %s\n"
        msg
        (cty_to_str expected)
        (cty_to_str te.expr_cty)
  in
  let common_eff ?(msg = "") eff1 eff2 =
    if effs_eq eff1 eff2 then eff1
    else if refined_eff_sub rctx term_vars eff1 eff2 then eff2
    else if refined_eff_sub rctx term_vars eff2 eff1 then eff1
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
  let extra_region_constraints = ref [] in
  let constraints_of_expr te = te.region_constraints in
  let constraints_of_exprs exprs =
    constraints_unions (List.map constraints_of_expr exprs)
  in
  let constraints_of_return_clause = function
    | None -> []
    | Some ({ return_body; _ } : Typed_ast.typed_return_clause) ->
      return_body.region_constraints
  in
  let constraints_of_hdl ({ op_body; _ } : Typed_ast.hdl) =
    op_body.region_constraints
  in
  let constraints_of_fundef ({ body; _ } : Typed_ast.fundef) =
    body.region_constraints
  in
  let constraints_of_desc = function
    | Unit | Var _ | Int _ | Float _ | Bool _ | Str _ | Char _ | Prim _ -> []
    | Arith (e1, _, e2)
    | Cmp (e1, _, e2)
    | BArith (e1, _, e2)
    | Get (e1, e2)
    | Resume (e1, e2)
    | ResumeFinal (e1, e2)
    | Stmt (e1, e2) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2]
    | Neg e -> constraints_of_expr e
    | App { func; args; _ } ->
      constraints_unions (constraints_of_expr func :: List.map constraints_of_expr args)
    | New exprs -> constraints_of_exprs exprs
    | Set (e1, e2, e3)
    | If (e1, e2, e3) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2; constraints_of_expr e3]
    | Raise { raise_args; _ } -> constraints_of_exprs raise_args
    | Handle { handle_body; return_clause; handler_defs; _ } ->
      constraints_unions [
        handle_body.region_constraints;
        constraints_of_return_clause return_clause;
        constraints_unions (List.map constraints_of_hdl handler_defs)
      ]
    | Recdef (fundefs, body) ->
      constraints_unions
        (body.region_constraints :: List.map constraints_of_fundef fundefs)
    | Fun { body; _ } -> body.region_constraints
    | Let (_, e1, e2) ->
      constraints_unions [constraints_of_expr e1; constraints_of_expr e2]
    | Typecon (_, _, args) -> constraints_of_exprs args
    | Match { match_expr; pattern_matching } ->
      constraints_unions
        (match_expr.region_constraints
         :: List.map (fun (_, e) -> e.region_constraints) pattern_matching)
    | TypeApp (_, e) -> constraints_of_expr e
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
        (* Equality on a refined operand should compare the underlying values,
           not the refinement metadata. Strip outer refinement so [e2] can be
           a plain int/bool. *)
        let ty = match ty_of e1' with TRefine (_, inner, _) -> inner | t -> t in
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
      let then_vars = branch_term_vars term_vars cond true in
      let else_vars = branch_term_vars term_vars cond false in
      let e1' = type_expr captured_vars cap_vars label_vars then_vars e1 in
      let e2' = type_expr captured_vars cap_vars label_vars else_vars e2 in
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
            region = RTop;
            return_cty = make_pure_cty return_ty
          }
        | None ->
          (
            match List.assoc_opt name prim_polymophic_sigs with
            | Some (tv, params_ty, return_ty) ->
              TForall (tv, KTy, [],
                TFun {
                  captured_set=None, Varset.empty;
                  cap_params=[];
                  label_params=[];
                  params_ty=List.map (fun ty -> (None, ty)) params_ty;
                  region = RTop;
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
                params_ty=List.map (fun ty -> (None, ty)) params_ty;
                region = RTop;
                return_cty = make_pure_cty return_ty
              }
          )
      ) in Prim f, make_pure_cty ty

    | Fun { captured_set; cap_params; label_params; params; body; return_cty } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      let params_ty = List.map (fun (x, ty) -> (Some x, ty)) params in
      (* Validate any refinement type written in this function's signature. The
         walk threads earlier params left-to-right so a later param's predicate
         can reference earlier ones. *)
      let _ = List.fold_left (fun acc (x, ty) ->
          check_refinement_wellformed rctx (acc @ term_vars) ty;
          acc @ [(x, ty)]
        ) [] params in
      (match return_cty with
       | CCty (rty, _) -> check_refinement_wellformed rctx (params @ term_vars) rty
       | _ -> ());
      (* The function body is typechecked at the annotated return_cty.
         Label parameters are abstract capabilities, so expose the operations
         from their declared effect signature under this function's answer
         context. This lets capability-parametric functions perform operations
         through a label parameter such as [r.ask].

         Outer cap_vars / label_vars are inherited so that the body can reference
         capabilities and labels that are lexically in scope (matching the Ott
         T_Abs rule, which checks the body under the full outer typing context).
         The function's own cap_params / label_params shadow outer entries with
         the same name. *)
      let label_param_answer =
        match return_cty with
        | CCty (_, EAns (_, c1, c2)) -> c1, c2
        | _ -> return_cty, return_cty
      in
      let inner_label_vars = List.map (fun (label, eff) ->
        let c1, c2 = label_param_answer in
        (label, make_label_binding ~op_captured_set:captured_set eff c1 c2)
      ) label_params in
      let body_cap_vars = cap_params @ cap_vars in
      let body_label_vars = inner_label_vars @ label_vars in
      let fun_region = fresh_function_region () in
      let body_rctx = {
        rctx with
        current_region = fun_region;
        flex_regions = constraints_union rctx.flex_regions [fun_region];
      } in
      let body' =
        check_cty_with ~msg:"Fun: Function body doesn't match the declared return cty"
          body_rctx captured_vars' body_cap_vars body_label_vars
          (params@term_vars) body return_cty
      in
      let inferred_region, residual_constraints =
        infer_function_region rctx fun_region body'.region_constraints
      in
      let body' = { body' with region_constraints = residual_constraints } in
      let ty = TFun {
        captured_set;
        cap_params;
        label_params;
        params_ty;
        region = inferred_region;
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
        | TFun {captured_set; cap_params; label_params; params_ty; region = fun_region; return_cty;_} ->
          (* Function regions are requirements: the current evaluation region
             must be at or below the region stored in the function type. *)
          (match func with
            | Prim _ -> ()
            | _ ->
              let region_constraints =
                check_region_requirement rctx rctx.current_region fun_region
              in
              extra_region_constraints :=
                constraints_union !extra_region_constraints region_constraints);
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
                    let app_args' = List.map2 (fun var (_, ty) -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args params_ty in
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
                let args_ty_expected = List.map (fun (name, ty) -> (name, substitute_to_type ty label_subs cap_subs)) params_ty in
                if not ((List.length app_args) = (List.length args_ty_expected)) then
                  typing_error "App: Incorrect number of arguments\n\tExpected: %d\n\tActual:%d" (List.length app_args) (List.length args_ty_expected)
                else
                  let app_args' = List.map2 (fun var (_, ty) -> check_ty captured_vars cap_vars label_vars term_vars var ty ~msg:"App: Argument types don't match") app_args args_ty_expected in
                  let substituted_cty = substitute_to_cty return_cty label_subs cap_subs in
                  let substituted_cty =
                    List.fold_left2
                      (fun acc (name, _) arg ->
                        match name with
                        | Some x -> substitute_term_to_cty acc x arg
                        | None -> acc)
                      substituted_cty args_ty_expected app_args
                  in
                  let substituted_ty = ty_of_cty substituted_cty in
                  let substituted_eff = eff_of_cty substituted_cty in
                  let call_eff = List.fold_left (fun acc te -> compose_effs acc (eff_of te)) (eff_of func') app_args' in
                  let eff = compose_effs call_eff substituted_eff in
                  App { func = func'; cap_insts; label_args; args = app_args' }, CCty (substituted_ty, eff)
          )
        | _ -> typing_error "App: Function type expected\n\tActual: %s" (type_to_str fun_type)
      )

    | Handle { captured_set; handle_body; handler_label; sig_name; handle_final; return_clause; handler_defs } ->
      let captured_vars' = check_capture_set_as_unamb captured_set captured_vars cap_vars label_vars in
      (* The handler label doubles as this handler's region. *)
      let handler_region = RVar handler_label in
      let body_rctx = {
        current_region = handler_region;
        subregions = (handler_region, rctx.current_region) :: rctx.subregions;
        label_regions = (handler_label, handler_region) :: rctx.label_regions;
        flex_regions = rctx.flex_regions;
        kind_env = rctx.kind_env;
      } in
      (* Per OTT (Handle rule, [lex_algeff_atm.ott:1389]), the return clause
         and operation handler clauses are typed at the OUTER region, not the
         handler's inner region. Only the handled body M is typed at the
         handler region. *)
      let handler_rctx = rctx in
      let c2 = handle_final in
      let has_full_ops =
        match List.assoc_opt sig_name !effect_sigs_context with
        | Some ops ->
          List.exists (function _, EffectFull _ -> true | _ -> false) ops
        | None -> false
      in
      let type_handler_defs handler_initial_cty =
        List.map (fun ({ op_anno; op_name; op_params; op_body }: SLsyntax.hdl) ->
          let effect_sig = get_effect_sig sig_name op_name in
          let op_bindings, effect_params, effect_inputs_ty, effect_return_ty, effect_return_var, op_c1, op_c2 =
            match effect_sig with
            | EffectSimple (effect_inputs_ty, effect_return_ty) ->
              ( [],
                List.map (fun ty -> (None, ty)) effect_inputs_ty,
                effect_inputs_ty,
                effect_return_ty,
                None,
                handler_initial_cty,
                c2 )
            | EffectFull (_bindings, params, op_cty) ->
              let op_cty =
                op_cty
                |> promote_cty_vars_in_cty _bindings
                |> replace_empty_fun_capture_cty captured_set
              in
              let cty_var_default = handler_cty_var_default c2 in
              let op_cty =
                List.fold_left
                  (fun acc (tv, kind) ->
                     match kind with
                     | KCty -> substitute_cty_var_to_cty acc tv cty_var_default
                     | _ -> acc)
                  op_cty _bindings
              in
              let return_ty, ans_binder, op_c1, op_c2 = split_op_cty op_name op_cty in
              ( _bindings,
                params,
                List.map erase_refinements_ty (op_param_tys params),
                erase_refinements_ty return_ty,
                ans_binder,
                op_c1,
                op_c2 )
          in
          let base_param_count = List.length effect_inputs_ty in
          let expected_param_count =
            base_param_count
            + if (op_anno = HHdl1) || (op_anno = HHdls) then 1 else 0
          in
          if List.length op_params <> expected_param_count then
            typing_error "Handle: Incorrect numbers of handler arguments\n\tExpected: %d\n\tActual: %d"
              expected_param_count (List.length op_params)
          else
            let rec take n xs =
              match n, xs with
              | 0, _ -> []
              | _, x :: rest -> x :: take (n - 1) rest
              | _ -> []
            in
            let op_arg_names = take base_param_count op_params in
            let substitute_op_params_cty c =
              List.fold_left2
                (fun acc (param_name, _) arg_name ->
                   match param_name with
                   | None -> acc
                   | Some x -> substitute_term_to_cty acc x (Var arg_name))
                c effect_params op_arg_names
            in
            let substitute_op_params_ty t =
              List.fold_left2
                (fun acc (param_name, _) arg_name ->
                   match param_name with
                   | None -> acc
                   | Some x -> substitute_term_to_type acc x (Var arg_name))
                t effect_params op_arg_names
            in
            let op_c1 = substitute_op_params_cty op_c1 in
            let op_c2 = substitute_op_params_cty op_c2 in
            let effect_return_ty = substitute_op_params_ty effect_return_ty in
            let op_c1 =
              if cty_contains_pred_app op_c1 then handler_initial_cty else op_c1
            in
            let op_c2 =
              if cty_contains_pred_app op_c2 then c2 else op_c2
            in
            let cont_type = TCont {
              captured_set;
              effect_return_var;
              effect_return_ty;
              return_cty = op_c1
            } in
            let effect_inputs_ty =
              if (op_anno = HHdl1) || (op_anno = HHdls)
              then effect_inputs_ty @ [cont_type]
              else effect_inputs_ty
            in
            let handler_params = List.map2 (fun x y -> (x, y)) op_params effect_inputs_ty in
            let op_rctx = { handler_rctx with kind_env = op_bindings @ handler_rctx.kind_env } in
            let op_body' =
              if op_anno = HDef then
                type_expr_with op_rctx captured_vars' [] [] (handler_params@term_vars) op_body
              else
                check_cty_with ~msg:"Handle: Effect body cty doesn't agree with the operation final answer type"
                  op_rctx captured_vars' [] [] (handler_params@term_vars) op_body op_c2
            in
            { op_anno; op_name; op_params; op_body = op_body' }
        ) handler_defs
      in
      if has_full_ops then begin
        (* Forward seeding: the handle annotation supplies C2. The handled body
           is typed first under C2 and synthesizes both its value type T and
           initial answer C1'. The return clause is typed afterwards using the
           synthesized T, producing C1. We then check that C1 is admissible
           for the body's synthesized initial answer under this checker's
           existing pure-to-impure answer subtyping. *)
        let provisional_c1 = CTyVar (fresh_handler_answer_var ()) in
        let body_label_vars =
          [(handler_label, make_label_binding ~op_captured_set:captured_set sig_name provisional_c1 c2)]
        in
        let preliminary_body =
          type_expr_with_final body_rctx captured_vars' []
            body_label_vars term_vars handle_body c2 None
        in
        let body_ty = ty_of preliminary_body in
        let preliminary_initial_binder, preliminary_initial_cty =
          initial_answer_family_of_threaded preliminary_body
        in
        let typed_return_clause, return_binder, return_cty =
          match return_clause with
          | None -> None, preliminary_initial_binder, preliminary_initial_cty
          | Some { return_var; return_var_ty; return_body } ->
            let return_var_ty =
              match return_var_ty with
              | None -> body_ty
              | Some annotated_ty ->
                if types_sub body_ty annotated_ty
                   || types_eq body_ty (erase_refinements_ty annotated_ty)
                   || check_refined_subtype body_rctx term_vars (Some handle_body) body_ty annotated_ty
                then annotated_ty
                else
                  typing_error "Handle: Body value type doesn't match return clause\n\tExpected: %s\n\tActual: %s\n"
                    (type_to_str annotated_ty) (type_to_str body_ty)
            in
            let return_body' =
              type_expr_with handler_rctx captured_vars' [] []
                ((return_var, return_var_ty) :: term_vars) return_body
            in
            let c1 = return_body'.expr_cty in
            ( Some ({ return_var; return_var_ty; return_cty = c1; return_body = return_body' }
                : Typed_ast.typed_return_clause),
              Some return_var,
              c1 )
        in
        let handle_body' =
          match return_clause with
          | None -> preliminary_body
          | Some _ ->
            type_expr_with_final body_rctx captured_vars' []
              body_label_vars term_vars handle_body c2
              (Some { initial_binder = return_binder; initial_cty = return_cty })
        in
        if List.exists (constraint_mentions handler_label) handle_body'.region_constraints then
          typing_error
            "Handle: residual subregion constraints cannot mention local region %s"
            handler_label;
        let body_actual_ty = ty_of handle_body' in
        if not (types_sub body_actual_ty body_ty
                || types_eq body_actual_ty (erase_refinements_ty body_ty)
                || check_refined_subtype body_rctx term_vars (Some handle_body) body_actual_ty body_ty)
        then
          typing_error "Handle: Body value type changed after inferring return clause\n\tExpected: %s\n\tActual: %s\n"
            (type_to_str body_ty) (type_to_str body_actual_ty);
        let body_initial_binder, body_initial_cty = initial_answer_family_of_threaded handle_body' in
        if not (answer_family_sub rctx term_vars body_ty
                  return_binder return_cty body_initial_binder body_initial_cty)
        then
          typing_error
            "Handle: Body initial answer type doesn't match return clause\n\tBody input: %s\n\tReturn answer: %s\n"
            (cty_to_str body_initial_cty)
            (cty_to_str return_cty);
        let handler_defs' = type_handler_defs return_cty in
        Handle { captured_set; handle_body = handle_body'; handler_label; sig_name; handle_final; return_clause = typed_return_clause; handler_defs = handler_defs' }, c2
      end else begin
      let body_ty, initial_ans_binder, initial_ans_cty, typed_return_clause =
        match return_clause with
        | None ->
          let t = ty_of_cty c2 in
          t, None, c2, None
        | Some { return_var; return_var_ty = Some return_var_ty; return_body } ->
          let return_body' =
            type_expr_with handler_rctx captured_vars' [] []
              ((return_var, return_var_ty) :: term_vars) return_body
          in
          let c1 = return_body'.expr_cty in
          ( return_var_ty,
            Some return_var,
            c1,
            Some ({ return_var; return_var_ty; return_cty = c1; return_body = return_body' }
              : Typed_ast.typed_return_clause) )
        | Some { return_var; return_var_ty = None; _ } ->
          typing_error
            "Handle: cannot infer return type for simple-effect handler return variable %s\n"
            return_var
      in
      let handler_defs' = type_handler_defs initial_ans_cty in
      (* Register per-op types (including the inferred ATM) in the label binding,
         so that every `Do x.op [...]` inside the body can look up its C1, C2. *)
      let lb = make_label_binding ~op_captured_set:captured_set sig_name initial_ans_cty c2 in
      let body_label_vars = [(handler_label, lb)] in
      let body_expected_cty = CCty (body_ty, EAns (initial_ans_binder, initial_ans_cty, c2)) in
      let handle_body' =
        check_cty_with ~msg:"Handle: Body doesn't match the handler's declared ATM"
          body_rctx captured_vars' []
          body_label_vars term_vars
          handle_body body_expected_cty
      in
      if List.exists (constraint_mentions handler_label) handle_body'.region_constraints then
        typing_error
          "Handle: residual subregion constraints cannot mention local region %s"
          handler_label;
      Handle { captured_set; handle_body = handle_body'; handler_label; sig_name; handle_final; return_clause = typed_return_clause; handler_defs = handler_defs' }, c2
      end

    | Raise { raise_label; raise_op; raise_tylikes; raise_atc; raise_args } ->
      let expr_desc, expr_cty, region_constraints =
        type_raise_expr rctx captured_vars cap_vars label_vars term_vars
          raise_label raise_op raise_tylikes raise_atc raise_args
      in
      extra_region_constraints :=
        constraints_union !extra_region_constraints region_constraints;
      expr_desc, expr_cty

    | Resume (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = ty_of cont' in
      (match type_cont with
      | TCont { effect_return_var; effect_return_ty; return_cty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        let return_cty =
          match effect_return_var with
          | Some x -> substitute_term_to_cty return_cty x arg
          | None -> return_cty
        in
        let eff = compose_effs (compose_effs (eff_of cont') (eff_of arg')) (eff_of_cty return_cty) in
        Resume (cont', arg'), CCty (ty_of_cty return_cty, eff)
      | _ -> typing_error "Resume: Continuation type expected\n"
      )

    | ResumeFinal (cont, arg) ->
      let cont' = type_expr captured_vars cap_vars label_vars term_vars cont in
      let type_cont = ty_of cont' in
      (match type_cont with
      | TCont { effect_return_var; effect_return_ty; return_cty; captured_set } ->
        check_capability captured_set captured_vars cap_vars label_vars;
        let arg' = check_ty captured_vars cap_vars label_vars term_vars arg effect_return_ty ~msg:"Resume: Argument doesn't match expected effect return type" in
        let return_cty =
          match effect_return_var with
          | Some x -> substitute_term_to_cty return_cty x arg
          | None -> return_cty
        in
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
          params_ty = List.map (fun (x, ty) -> (Some x, ty)) params;
          region = rctx.current_region;
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
      (* A refined scrutinee (e.g. the result of a [resume] whose answer type
         carries a refinement) is matched on its underlying datatype. *)
      let match_expr_ty = strip_outer_refinement (ty_of match_expr') in
      (* Encodable form of the scrutinee value, used to relate the bound pattern
         variables to the scrutinee's refinement: inside the [C(y..)] branch we
         learn [scrutinee == C(y..)], so the SMT can propagate the scrutinee's
         refinement onto the payload variables (e.g. matching [Success y] on a
         scrutinee refined to [Success 5] yields [y == 5]). *)
      let scrut_term = pred_term_of_expr_opt match_expr in
      let branch_hyp_vars type_con arg_names =
        match scrut_term with
        | Some s ->
          let rhs = PTCon (type_con, List.map (fun a -> PTVar a) arg_names) in
          [ (fresh_refinement_witness (),
             TRefine ("__match", TBool, PCmp (s, CEq, rhs))) ]
        | None -> []
      in
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
                      let branch_vars = arg_vars @ branch_hyp_vars type_con args @ term_vars in
                      let res' = type_expr captured_vars cap_vars label_vars branch_vars res in
                      let res_ty = ty_of res' in
                      let pattern_matching_rest' = List.map (fun (pt, res) -> (match pt with
                      | Syntax__Common.PTypecon (type_con, args) ->
                        let params_type = List.map (fun ty -> substitute_ty ty type_subs) (List.assoc type_con type_cons) in
                        let arg_vars = List.map2 (fun arg ty -> (arg, ty)) args params_type in
                        let branch_vars = arg_vars @ branch_hyp_vars type_con args @ term_vars in
                        let res' = check_ty captured_vars cap_vars label_vars branch_vars res res_ty ~msg:"Match: The types of clauses don't match" in
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
        | TForall (tv, kind, constraints, t') ->
          let tylike_arg = TLTy t_arg in
          (match kind, tylike_arg with
          | KTy, TLTy _ -> ()
          | KReg, _ when region_of_tylike tylike_arg <> None -> ()
          | _ ->
            typing_error
              "TypeApp: type argument has wrong kind for %s"
              tv);
          let instantiated_constraints =
            List.map
              (fun c -> substitute_tylike_to_constraint c tv kind tylike_arg)
              constraints
          in
          let residual_constraints =
            constraints_unions
              (List.map (check_subregion_constraint rctx) instantiated_constraints)
          in
          extra_region_constraints :=
            constraints_union !extra_region_constraints residual_constraints;
          substitute_tylike_to_type t' tv kind tylike_arg
        | _ -> typing_error "TypeApp: A forall type expected\n\tActual: %s" (type_to_str te_ty)
      ) in
      TypeApp (t_arg, te), CCty (instantiated_ty, eff_of te)


  in
  let region_constraints =
    constraints_union !extra_region_constraints (constraints_of_desc expr_desc)
  in
  { expr_desc; expr_cty; region_constraints; captured_vars; cap_vars; label_vars }


let check_type_defs (defs: typedef list) =
  let type_names' = (List.map fst !type_defs_context)@(List.map (fun { type_name; _ } -> type_name) defs) in

  let rec check_typedef_ty type_names type_vars ty =
    match ty with
    | TRef ty' -> check_typedef_ty type_names type_vars ty'
    | TFun { params_ty; return_cty; _ } ->
      List.iter (fun (_, ty) -> check_typedef_ty type_names type_vars ty) params_ty;
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
    | TForall (tv, _kind, _constraints, ty') -> check_typedef_ty type_names (tv::type_vars) ty'
    | TRefine (_, inner, _) -> check_typedef_ty type_names type_vars inner
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
    | EAns (_, c1, c2) ->
      check_typedef_cty type_names type_vars c1;
      check_typedef_cty type_names type_vars c2
    | EEffVar _ -> ()
  in

  List.iter (fun { type_name=_; type_params; type_cons } ->
    List.iter (fun (_, args_ty) ->
      List.iter (check_typedef_ty type_names' type_params) args_ty
    ) type_cons
  ) defs
