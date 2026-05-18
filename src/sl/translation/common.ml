open SLsyntax

module Varset = Syntax__Varset
module Varmap = Map.Make(String)

exception Error of (string * string)

let print_message msg_type =
    Format.eprintf "%s: " msg_type ;
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

let print_error (err_type, msg) = print_message err_type "%s" msg

let error ?(kind="Error") =
  let k _ =
    let msg = Format.flush_str_formatter () in
      print_error (kind, msg);
      raise (Error (kind, msg))
  in
    Format.kfprintf k Format.str_formatter

let typing_error msg =
  error ~kind:"Type error" msg

(** Finds the label binding for a given label. *)
let find_label_binding_opt label captured_vars label_vars =
  match List.assoc_opt label label_vars with
  | Some lb -> Some lb
  | None ->
    match captured_vars with
    | Labels captured_labels -> List.assoc_opt label captured_labels
    | _ -> None

let find_label_binding label captured_vars label_vars =
  match find_label_binding_opt label captured_vars label_vars with
  | Some lb -> lb
  | None -> typing_error "Label %s not found\n" label

(** Finds the effect name associated to the label. *)
let find_effect_name_opt label captured_vars label_vars =
  Option.map (fun lb -> lb.lb_effect_name) (find_label_binding_opt label captured_vars label_vars)

let find_effect_name label captured_vars label_vars =
  match find_effect_name_opt label captured_vars label_vars with
  | Some eff_name -> eff_name
  | None -> typing_error "Label %s not found\n" label

(** Pretty-prints refinement logic terms and formulas. *)
let rec pred_term_to_str (t: pred_term) =
  match t with
  | PTUnit -> "()"
  | PTInt n -> string_of_int n
  | PTBool true -> "true"
  | PTBool false -> "false"
  | PTVar x -> x
  | PTArith (t1, op, t2) ->
    let s = match op with
      | AAdd -> "+" | ASub -> "-" | AMult -> "*" | ADiv -> "/" | AMod -> "%"
    in Printf.sprintf "(%s %s %s)" (pred_term_to_str t1) s (pred_term_to_str t2)

and pred_to_str (p: pred) =
  match p with
  | PAtom t -> pred_term_to_str t
  | PCmp (t1, op, t2) ->
    let s = match op with
      | CEq -> "==" | CNeq -> "!=" | CLt -> "<" | CGt -> ">" | CLe -> "<=" | CGe -> ">="
    in Printf.sprintf "(%s %s %s)" (pred_term_to_str t1) s (pred_term_to_str t2)
  | PApp (p_name, args) ->
    Printf.sprintf "%s(%s)" p_name (String.concat ", " (List.map pred_term_to_str args))
  | PBArith (p1, op, p2) ->
    let s = match op with BConj -> "&&" | BDisj -> "||" in
    Printf.sprintf "(%s %s %s)" (pred_to_str p1) s (pred_to_str p2)
  | PNeg p' -> Printf.sprintf "(! %s)" (pred_to_str p')

let rec pred_term_of_expr_opt (e: expr) : pred_term option =
  match e with
  | Unit -> Some PTUnit
  | Int n -> Some (PTInt n)
  | Bool b -> Some (PTBool b)
  | Var x -> Some (PTVar x)
  | Arith (e1, op, e2) ->
    (match pred_term_of_expr_opt e1, pred_term_of_expr_opt e2 with
     | Some t1, Some t2 -> Some (PTArith (t1, op, t2))
     | _ -> None)
  | _ -> None

let rec rename_var_in_pred_term (t: pred_term) (old_v: string) (new_v: string) : pred_term =
  match t with
  | PTVar x -> if x = old_v then PTVar new_v else t
  | PTUnit | PTInt _ | PTBool _ -> t
  | PTArith (t1, op, t2) ->
    PTArith (rename_var_in_pred_term t1 old_v new_v, op, rename_var_in_pred_term t2 old_v new_v)

and rename_var_in_pred (p: pred) (old_v: string) (new_v: string) : pred =
  match p with
  | PAtom t -> PAtom (rename_var_in_pred_term t old_v new_v)
  | PCmp (t1, op, t2) ->
    PCmp (rename_var_in_pred_term t1 old_v new_v, op, rename_var_in_pred_term t2 old_v new_v)
  | PApp (p_name, args) ->
    PApp (p_name, List.map (fun arg -> rename_var_in_pred_term arg old_v new_v) args)
  | PBArith (p1, op, p2) ->
    PBArith (rename_var_in_pred p1 old_v new_v, op, rename_var_in_pred p2 old_v new_v)
  | PNeg p' -> PNeg (rename_var_in_pred p' old_v new_v)

let rec subst_var_in_pred_term (t: pred_term) (old_v: string) (witness: pred_term) : pred_term =
  match t with
  | PTVar x -> if x = old_v then witness else t
  | PTUnit | PTInt _ | PTBool _ -> t
  | PTArith (t1, op, t2) ->
    PTArith (subst_var_in_pred_term t1 old_v witness, op, subst_var_in_pred_term t2 old_v witness)

and subst_var_in_pred (p: pred) (old_v: string) (witness: pred_term) : pred =
  match p with
  | PAtom t -> PAtom (subst_var_in_pred_term t old_v witness)
  | PCmp (t1, op, t2) ->
    PCmp (subst_var_in_pred_term t1 old_v witness, op, subst_var_in_pred_term t2 old_v witness)
  | PApp (p_name, args) ->
    PApp (p_name, List.map (fun arg -> subst_var_in_pred_term arg old_v witness) args)
  | PBArith (p1, op, p2) ->
    PBArith (subst_var_in_pred p1 old_v witness, op, subst_var_in_pred p2 old_v witness)
  | PNeg p' -> PNeg (subst_var_in_pred p' old_v witness)

(** Substitute a value expression into a predicate. Returns [None] only when
    the variable actually occurs and the witness is outside the logical
    predicate syntax. *)
let rec subst_expr_in_pred (p: pred) (old_v: string) (witness: expr) : pred option =
  let subst_term t =
    match pred_term_of_expr_opt witness with
    | Some witness -> Some (subst_var_in_pred_term t old_v witness)
    | None ->
      let rec occurs t =
        match t with
        | PTVar x -> x = old_v
        | PTArith (t1, _, t2) -> occurs t1 || occurs t2
        | PTUnit | PTInt _ | PTBool _ -> false
      in
      if occurs t then None else Some t
  in
  match p with
  | PAtom t ->
    (match subst_term t with
     | Some t' -> Some (PAtom t')
     | None -> None)
  | PCmp (t1, op, t2) ->
    (match subst_term t1, subst_term t2 with
     | Some t1', Some t2' -> Some (PCmp (t1', op, t2'))
     | _ -> None)
  | PApp (p_name, args) ->
    let rec subst_args args =
      match args with
      | [] -> Some []
      | arg :: rest ->
        (match subst_term arg, subst_args rest with
         | Some arg', Some rest' -> Some (arg' :: rest')
         | _ -> None)
    in
    (match subst_args args with
     | Some args' -> Some (PApp (p_name, args'))
     | None -> None)
  | PBArith (p1, op, p2) ->
    (match subst_expr_in_pred p1 old_v witness, subst_expr_in_pred p2 old_v witness with
     | Some p1', Some p2' -> Some (PBArith (p1', op, p2'))
     | _ -> None)
  | PNeg p' ->
    (match subst_expr_in_pred p' old_v witness with
     | Some p'' -> Some (PNeg p'')
     | None -> None)

let rec op_parameter_to_str (name, ty) =
  match name with
  | Some x -> Printf.sprintf "%s: %s" x (type_to_str ty)
  | None -> type_to_str ty

(** Returns a formatted string for the given type. For debugging purpose. *)
and type_to_str (ty: ty) =
  let capability_to_str (cap: capability) =
    match cap with
    | Some cap_var, labels -> Printf.sprintf "%s; %s" cap_var (String.concat ", " (Varset.to_list labels))
    | None, labels -> String.concat ", " (Varset.to_list labels)
  in
  match ty with
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TFloat -> "float"
  | TChar -> "char"
  | TStr -> "string"
  | TRef ty' -> Printf.sprintf "ref %s" (type_to_str ty')
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let captured_set_str = capability_to_str captured_set in
    let cap_params_str = String.concat ", " cap_params in
    let label_vars_str = String.concat ", " (List.map (fun (label, eff) -> Printf.sprintf "%s: %s" label eff) label_params) in
    let params_ty_str = String.concat ", " (List.map op_parameter_to_str params_ty) in
    let region_str = match region with
      | RTop -> "⊤"
      | RVar v -> v
      | RNull -> "null"
    in
    Printf.sprintf "{ %s } [%s; %s] (%s) -> [%s] %s" captured_set_str cap_params_str label_vars_str params_ty_str region_str (cty_to_str return_cty)
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    let captured_set_str = capability_to_str captured_set in
    let effect_ty_str =
      match effect_return_var with
      | Some x -> Printf.sprintf "%s: %s" x (type_to_str effect_return_ty)
      | None -> type_to_str effect_return_ty
    in
    Printf.sprintf "{ %s } cont %s -> %s" captured_set_str effect_ty_str (cty_to_str return_cty)
  | TNode t -> Printf.sprintf "node_t::[%s]" (type_to_str t)
  | TTree t -> Printf.sprintf "tree_t::[%s]" (type_to_str t)
  | TQueue t -> Printf.sprintf "queue_t::[%s]" (type_to_str t)
  | TArray t -> Printf.sprintf "array_t::[%s]" (type_to_str t)
  | TCon (t, t_args) ->
    if (t_args = [])
      then t else Printf.sprintf "%s::[%s]" t (String.concat ", " (List.map type_to_str t_args))
  | TVar v -> v
  | TForall (tvar, _kind, constraints, ty') ->
    let region_to_str = function
      | RTop -> "⊤"
      | RVar v -> v
      | RNull -> "null"
    in
    let rec distance_to_str = function
      | DZero -> "lzero"
      | DOne -> "lone"
      | DVar v -> v
      | DPlus (d1, d2) -> Printf.sprintf "(%s + %s)" (distance_to_str d1) (distance_to_str d2)
    in
    let constraint_to_str { rc_inner; rc_dist; rc_outer } =
      Printf.sprintf "%s <=[%s] %s"
        (region_to_str rc_inner)
        (distance_to_str rc_dist)
        (region_to_str rc_outer)
    in
    let constraints_str =
      match constraints with
      | [] -> ""
      | _ -> Printf.sprintf "[%s]" (String.concat ", " (List.map constraint_to_str constraints))
    in
    Printf.sprintf "∀%s%s. %s" tvar constraints_str (type_to_str ty')
  | TCap (_region, _opty) -> "Cap"
  | TRefine (v, inner, p) ->
    Printf.sprintf "{%s: %s | %s}" v (type_to_str inner) (pred_to_str p)

and cty_to_str (c: cty) =
  match c with
  | CTyVar v -> v
  | CCty (t, EPure) -> type_to_str t
  | CCty (t, e) -> Printf.sprintf "%s / %s" (type_to_str t) (eff_to_str e)
  | CFill (v, c') -> Printf.sprintf "%s<%s>" v (cty_to_str c')

and eff_to_str (e: eff) =
  match e with
  | EPure -> "pure"
  | EAns (None, c1, c2) -> Printf.sprintf "(%s) => (%s)" (cty_to_str c1) (cty_to_str c2)
  | EAns (Some x, c1, c2) -> Printf.sprintf "(%s. %s) => (%s)" x (cty_to_str c1) (cty_to_str c2)
  | EEffVar v -> v

let rec distance_to_str (d: distance) =
  match d with
  | DZero -> "lzero"
  | DOne -> "lone"
  | DVar v -> v
  | DPlus (d1, d2) -> Printf.sprintf "(%s + %s)" (distance_to_str d1) (distance_to_str d2)

let rec atc_to_str (cc: atc) =
  match cc with
  | ATCHole -> "[]"
  | ATCVar v -> v
  | ATCAns (t, c, cc') ->
    Printf.sprintf "%s / %s => %s" (type_to_str t) (cty_to_str c) (atc_to_str cc')
  | ATCFill (x, cc') -> Printf.sprintf "%s[%s]" x (atc_to_str cc')

(** Type equality **)

(** Applies alpha-normalization to the given type. *)
let alpha_normalize ty =
  let fresh_label =
    let counter = ref 0 in
    fun () ->
      let label = "__a" ^ (string_of_int !counter) in
      incr counter;
      label
  in
  let fresh_var =
    let counter = ref 0 in
    fun () ->
      let var = "'__t" ^ (string_of_int !counter) in
      incr counter;
      var
  in
  let fresh_refine =
    let counter = ref 0 in
    fun () ->
      let v = "__r" ^ (string_of_int !counter) in
      incr counter;
      v
  in
  let fresh_term =
    let counter = ref 0 in
    fun () ->
      let v = "__x" ^ (string_of_int !counter) in
      incr counter;
      v
  in
  let rename_captured_set captured_set env =
    let (cap_opt, labels) = captured_set in
    let labels' = Varset.map (fun label ->
      match Varmap.find_opt label env with
      | Some label' -> label'
      | None -> label
    ) labels in
    match cap_opt with
    | Some cap ->
      (match Varmap.find_opt cap env with
      | Some cap' -> Some cap', labels'
      | None -> Some cap, labels')
    | None -> None, labels'
  in
  let rename_region env = function
    | RVar v -> (match Varmap.find_opt v env with Some v' -> RVar v' | None -> RVar v)
    | r -> r
  in
  let rec rename_distance env = function
    | DVar v -> (match Varmap.find_opt v env with Some v' -> DVar v' | None -> DVar v)
    | DPlus (d1, d2) -> DPlus (rename_distance env d1, rename_distance env d2)
    | d -> d
  in
  let rename_constraint env { rc_inner; rc_dist; rc_outer } =
    {
      rc_inner = rename_region env rc_inner;
      rc_dist = rename_distance env rc_dist;
      rc_outer = rename_region env rc_outer;
    }
  in
  let rec rename_term_in_type ty old_v new_v =
    match ty with
    | TRef t -> TRef (rename_term_in_type t old_v new_v)
    | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
      let rec go_params params =
        match params with
        | [] -> []
        | (Some x, t) :: rest when x = old_v ->
          (Some x, rename_term_in_type t old_v new_v) :: rest
        | (name, t) :: rest ->
          (name, rename_term_in_type t old_v new_v) :: go_params rest
      in
      let shadows =
        List.exists (function Some x, _ when x = old_v -> true | _ -> false) params_ty
      in
      TFun {
        captured_set;
        cap_params;
        label_params;
        params_ty = go_params params_ty;
        region;
        return_cty =
          if shadows then return_cty else rename_term_in_cty return_cty old_v new_v
      }
    | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
      TCont {
        captured_set;
        effect_return_var;
        effect_return_ty = rename_term_in_type effect_return_ty old_v new_v;
        return_cty =
          if effect_return_var = Some old_v
            then return_cty
            else rename_term_in_cty return_cty old_v new_v
      }
    | TNode t -> TNode (rename_term_in_type t old_v new_v)
    | TTree t -> TTree (rename_term_in_type t old_v new_v)
    | TQueue t -> TQueue (rename_term_in_type t old_v new_v)
    | TArray t -> TArray (rename_term_in_type t old_v new_v)
    | TCon (name, args) -> TCon (name, List.map (fun t -> rename_term_in_type t old_v new_v) args)
    | TForall (tv, kind, constraints, t) ->
      TForall (tv, kind, constraints, rename_term_in_type t old_v new_v)
    | TRefine (v, inner, p) when v = old_v ->
      TRefine (v, rename_term_in_type inner old_v new_v, p)
    | TRefine (v, inner, p) ->
      TRefine (v, rename_term_in_type inner old_v new_v, rename_var_in_pred p old_v new_v)
    | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty
  and rename_term_in_cty c old_v new_v =
    match c with
    | CTyVar _ -> c
    | CCty (t, e) -> CCty (rename_term_in_type t old_v new_v, rename_term_in_eff e old_v new_v)
    | CFill (v, c') -> CFill (v, rename_term_in_cty c' old_v new_v)
  and rename_term_in_eff e old_v new_v =
    match e with
    | EPure | EEffVar _ -> e
    | EAns (Some x, c1, c2) when x = old_v ->
      EAns (Some x, c1, rename_term_in_cty c2 old_v new_v)
    | EAns (x, c1, c2) ->
      EAns (x, rename_term_in_cty c1 old_v new_v, rename_term_in_cty c2 old_v new_v)
  in
  let rec normalize ty env =
    match ty with
    | TRef ty' -> TRef (normalize ty' env)
    | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
      let captured_set' = rename_captured_set captured_set env in
      let new_caps = List.map (fun _ -> fresh_label()) cap_params in
      let new_labels = List.map (fun (_, eff) -> (fresh_label(), eff)) label_params in
      let env' = List.fold_right2 (fun label label_new env ->
        Varmap.add label label_new env
      ) (cap_params@(List.map fst label_params)) (new_caps@(List.map fst new_labels)) env in
      let region' = match region with
        | RVar v -> (match Varmap.find_opt v env' with Some v' -> RVar v' | None -> RVar v)
        | _ -> region
      in
      let rec normalize_params params return_cty =
        match params with
        | [] -> [], normalize_cty return_cty env'
        | (None, ty) :: rest ->
          let rest', return_cty' = normalize_params rest return_cty in
          (None, normalize ty env') :: rest', return_cty'
        | (Some x, ty) :: rest ->
          let x_new = fresh_term () in
          let rest' =
            List.map
              (fun (name, t) -> (name, rename_term_in_type t x x_new))
              rest
          in
          let return_cty' = rename_term_in_cty return_cty x x_new in
          let rest'', return_cty'' = normalize_params rest' return_cty' in
          (Some x_new, normalize ty env') :: rest'', return_cty''
      in
      let params_ty', return_cty' = normalize_params params_ty return_cty in
      TFun {
        captured_set = captured_set';
        cap_params = new_caps;
        label_params = new_labels;
        params_ty = params_ty';
        region = region';
        return_cty = return_cty'
      }
    | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
      TCont {
        captured_set = rename_captured_set captured_set env;
        effect_return_var;
        effect_return_ty = normalize effect_return_ty env;
        return_cty = normalize_cty return_cty env
      }
    | TCon (tv, t_args) ->
      let t_args' = List.map (fun t_arg -> normalize t_arg env) t_args in
      TCon (tv, t_args')
    | TVar tv -> (match Varmap.find_opt tv env with
      | Some tv' -> TVar tv'
      | None -> TVar tv
    )
    | TForall (tv, kind, constraints, ty') ->
      let tv_new = fresh_var() in
      let env' = Varmap.add tv tv_new env in
      TForall (tv_new, kind, List.map (rename_constraint env') constraints, (normalize ty' env'))
    | TRefine (v, inner, p) ->
      (* Alpha-rename the value binder to a stable fresh name; rename free
         occurrences of [v] inside the predicate. Capture/label envs don't
         affect the predicate (predicates only reference term variables),
         so [env] is not threaded into [p]. *)
      let v_new = fresh_refine () in
      let p' = rename_var_in_pred p v v_new in
      TRefine (v_new, normalize inner env, p')
    | _ -> ty
  and normalize_cty c env =
    match c with
    | CTyVar v -> (match Varmap.find_opt v env with Some v' -> CTyVar v' | None -> CTyVar v)
    | CCty (t, e) -> CCty (normalize t env, normalize_eff e env)
    | CFill (v, c') -> CFill (v, normalize_cty c' env)
  and normalize_eff e env =
    match e with
    | EPure -> EPure
    | EAns (x, c1, c2) -> EAns (x, normalize_cty c1 env, normalize_cty c2 env)
    | EEffVar v -> EEffVar v
  in
  normalize ty Varmap.empty

let alpha_normalize_cty (c: cty) =
  (* Round-trip through a wrapper type to reuse the same fresh counters. *)
  match c with
  | CCty (t, EPure) -> CCty (alpha_normalize t, EPure)
  | _ ->
    (* Good enough for now: normalize each ty component separately. *)
    let rec go c = match c with
      | CTyVar _ -> c
      | CCty (t, e) -> CCty (alpha_normalize t, go_eff e)
      | CFill (v, c') -> CFill (v, go c')
    and go_eff e = match e with
      | EPure -> EPure
      | EAns (x, c1, c2) -> EAns (x, go c1, go c2)
      | EEffVar _ -> e
    in go c

(** Checks for equality of two types. *)
let optional_param_names_compatible name1 name2 =
  match name1, name2 with
  | Some x, Some y -> x = y
  | _ -> true

let types_eq t1 t2 =
  let captured_sets_eq cs1 cs2 =
    match cs1, cs2 with
    | (None, labels1), (None, labels2) -> Varset.equal labels1 labels2
    | (Some cap1, labels1), (Some cap2, labels2) -> (cap1 = cap2) && (Varset.equal labels1 labels2)
    | _ -> false
  in
  let constraints_eq c1 c2 =
    List.equal (=) (List.sort_uniq compare c1) (List.sort_uniq compare c2)
  in
  let rec normalized_types_eq t1 t2 =
    match (t1, t2) with
    | (TRef t1', TRef t2') -> normalized_types_eq t1' t2'
    | (TFun {captured_set = cs; cap_params = cp; label_params=lp; params_ty = pt; region = r; return_cty = rc},
      TFun {captured_set = cs'; cap_params = cp'; label_params = lp'; params_ty = pt' ; region = r'; return_cty = rc'}) ->
        (captured_sets_eq cs cs')
        && (cp = cp')
        && (lp = lp')
        && (r = r')
        && (List.equal
              (fun (name1, t1) (name2, t2) ->
                optional_param_names_compatible name1 name2 && normalized_types_eq t1 t2)
              pt pt')
        && (normalized_ctys_eq rc rc')
    | (TCont { captured_set = cs; effect_return_var = erv; effect_return_ty = et; return_cty = rc },
      TCont { captured_set = cs'; effect_return_var = erv'; effect_return_ty = et'; return_cty = rc' }) ->
        (captured_sets_eq cs cs')
        && erv = erv'
        && (normalized_types_eq et et')
        && (normalized_ctys_eq rc rc')
    | (TCon (tv1, t1_args), TCon (tv2, t2_args)) ->
      (tv1 = tv2) && (List.equal normalized_types_eq t1_args t2_args)
    | (TForall (v1, k1, c1, t1'), TForall (v2, k2, c2, t2')) ->
      v1 = v2 && k1 = k2 && constraints_eq c1 c2 && (normalized_types_eq t1' t2')
    | (TRefine (v1, t1', p1), TRefine (v2, t2', p2)) ->
      (* alpha_normalize already renamed both binders via the same fresh
         counter, so equal-shape refinements have v1 = v2 and the predicate
         expressions can be compared structurally. *)
      v1 = v2 && normalized_types_eq t1' t2' && p1 = p2
    | (t1', t2') -> t1' = t2'
  and normalized_ctys_eq c1 c2 =
    match c1, c2 with
    | CTyVar v1, CTyVar v2 -> v1 = v2
    | CCty (t1, e1), CCty (t2, e2) ->
      normalized_types_eq t1 t2 && normalized_effs_eq e1 e2
    | CFill (v1, c1'), CFill (v2, c2') -> v1 = v2 && normalized_ctys_eq c1' c2'
    | _ -> false
  and normalized_effs_eq e1 e2 =
    match e1, e2 with
    | EPure, EPure -> true
    | EAns (x1, c1a, c2a), EAns (x2, c1b, c2b) ->
      x1 = x2 &&
      normalized_ctys_eq c1a c1b && normalized_ctys_eq c2a c2b
    | EEffVar v1, EEffVar v2 -> v1 = v2
    | _ -> false
  in
  let t1_normalized = alpha_normalize t1 in
  let t2_normalized = alpha_normalize t2 in
  normalized_types_eq t1_normalized t2_normalized

(** Structural equality on computation types. *)
let ctys_eq c1 c2 =
  let rec go c1 c2 =
    match c1, c2 with
    | CTyVar v1, CTyVar v2 -> v1 = v2
    | CCty (t1, e1), CCty (t2, e2) -> types_eq t1 t2 && eff_go e1 e2
    | CFill (v1, c1'), CFill (v2, c2') -> v1 = v2 && go c1' c2'
    | _ -> false
  and eff_go e1 e2 =
    match e1, e2 with
    | EPure, EPure -> true
    | EAns (x1, a1, b1), EAns (x2, a2, b2) -> x1 = x2 && go a1 a2 && go b1 b2
    | EEffVar v1, EEffVar v2 -> v1 = v2
    | _ -> false
  in go c1 c2

let effs_eq e1 e2 =
  match e1, e2 with
  | EPure, EPure -> true
  | EAns (x1, a1, b1), EAns (x2, a2, b2) -> x1 = x2 && ctys_eq a1 a2 && ctys_eq b1 b2
  | EEffVar v1, EEffVar v2 -> v1 = v2
  | _ -> false

let cty_eq = ctys_eq
let eff_eq = effs_eq

(** Normalizes a level by treating DZero as the identity for DPlus. Right-
    associates DPlus so the result is a flat list of non-zero atoms joined
    by DPlus. Does not attempt commutativity — the ott's ==lv includes it
    implicitly but this level of normalization matches existing use. *)
let normalize_distance d =
  let rec collect d acc =
    match d with
    | DZero -> acc
    | DPlus (a, b) -> collect a (collect b acc)
    | _ -> d :: acc
  in
  match collect d [] with
  | [] -> DZero
  | [d'] -> d'
  | d' :: rest -> List.fold_left (fun acc x -> DPlus (acc, x)) d' rest

(** Equality on levels modulo DZero identity (matches the ott's ==lv except
    for commutativity, which no rule today relies on). *)
let distance_eq d1 d2 =
  let rec eq d1 d2 =
    match d1, d2 with
    | DZero, DZero -> true
    | DOne, DOne -> true
    | DVar v1, DVar v2 -> v1 = v2
    | DPlus (a1, b1), DPlus (a2, b2) -> eq a1 a2 && eq b1 b2
    | _ -> false
  in
  eq (normalize_distance d1) (normalize_distance d2)

(** CC{C}: fill the hole of an ATC with the given computation type.
    Implements the denotational semantics used in the T_Do rule of the ott. *)
let rec fill_atc (cc: atc) (c: cty) : cty =
  match cc with
  | ATCHole -> c
  | ATCVar x -> CFill (x, c)
  | ATCAns (t, c', cc') ->
    CCty (t, EAns (None, c', fill_atc cc' c))
  | ATCFill (x, cc') -> CFill (x, fill_atc cc' c)

let rec captured_set_sub cs1 cs2 =
  match cs1, cs2 with
  | (None, labels1), (None, labels2) -> Varset.subset labels1 labels2
  | (Some cap1, labels1), (Some cap2, labels2) ->
    cap1 = cap2 && Varset.subset labels1 labels2
  | _ -> false

(** A lightweight subtype relation used by ATM composition.
    Today it mostly exists to treat functions/continuations with a smaller
    captured set as subtypes of ones that mention more ambient labels.
    [kind_env] is threaded so the S_CAbs case can verify G |- X : ATC(l)
    when reachable variables have explicit kind bindings. Defaults to []
    (assume-well-formed) for existing callers. *)
and types_sub ?(kind_env=[]) t1 t2 =
  if types_eq t1 t2 then true
  else
    let t1 = alpha_normalize t1 in
    let t2 = alpha_normalize t2 in
    match t1, t2 with
    | TFun { captured_set = cs1; cap_params = cp1; label_params = lp1; params_ty = pt1; region = r1; return_cty = rc1 },
      TFun { captured_set = cs2; cap_params = cp2; label_params = lp2; params_ty = pt2; region = r2; return_cty = rc2 } ->
      captured_set_sub cs1 cs2
      && cp1 = cp2
      && lp1 = lp2
      && r1 = r2
      && List.length pt1 = List.length pt2
      && List.for_all2 (fun (actual_name, actual_param) (expected_name, expected_param) ->
           optional_param_names_compatible actual_name expected_name
           && types_sub ~kind_env expected_param actual_param
         ) pt1 pt2
      && cty_sub ~kind_env rc1 rc2
    | TCont { captured_set = cs1; effect_return_var = erv1; effect_return_ty = et1; return_cty = rc1 },
      TCont { captured_set = cs2; effect_return_var = erv2; effect_return_ty = et2; return_cty = rc2 } ->
      captured_set_sub cs1 cs2
      && erv1 = erv2
      && types_eq et1 et2
      && cty_sub ~kind_env rc1 rc2
    (* Refinement: dropping a refinement is always sound, [{v:T|P} <: T]. *)
    | TRefine (_, inner, _), _ -> types_sub ~kind_env inner t2
    (* The other directions ([T <: {v:T|P}] and [{v:T|Q} <: {v:T|P}]) require
       VC discharge; that lives in [Refinement.check_subtype_with_vc] reached
       through [check_ty]/[check_cty]. The pure-structural [types_sub] is
       conservative: it returns false here. *)
    | _ -> false

(** Subtyping on effects / computation types, following the simplified ott
    rules plus the lightweight capture-set subtyping above. *)
and eff_sub ?(kind_env=[]) e1 e2 =
  effs_eq e1 e2
  ||
  match e1, e2 with
  | EPure, _ -> true
  | EAns (x1, c11, c12), EAns (x2, c21, c22) ->
    x1 = x2 &&
    cty_sub ~kind_env c21 c11 && cty_sub ~kind_env c12 c22
  | _ -> false

(** Composes two effects as they appear in sequenced computations.
    If [e1] is the effect of the computation that runs first (M1) and [e2] is
    the effect of the computation that runs second (M2), the combined effect
    follows the T_LetImpure rule in the binder-free form:
      M1 : T1 / C => C1
      M2 : T2 / C2 => C
      ===> M1 ; M2 : T2 / C2 => C1
    Note the cty C must match across the two effects. *)
and compose_effs ?(kind_env=[]) e1 e2 =
  match e1, e2 with
  | EPure, _ -> e2
  | _, EPure -> e1
  | EAns (_x1, c_in_1, c_out_1), EAns (x2, c_in_2, c_out_2) ->
    if cty_sub ~kind_env c_out_2 c_in_1
      then EAns (x2, c_in_2, c_out_1)
      else typing_error "Effect composition: answer types disagree\n\tFirst needs input %s, second produces output %s\n"
        (cty_to_str c_in_1) (cty_to_str c_out_2)
  | _ ->
    typing_error "Effect composition: cannot compose %s with %s\n"
      (eff_to_str e1) (eff_to_str e2)

and cty_sub ?(kind_env=[]) c1 c2 =
  if ctys_eq c1 c2 then true
  else
    match c1, c2 with
    | CCty (t1, e1), CCty (t2, e2) ->
      types_sub ~kind_env t1 t2 && eff_sub ~kind_env e1 e2
    (* S_CAbs: X[C1] <: X[C2] when C1 <: C2 and G |- X : ATC(l).
       The kinding premise is checked when X is in [kind_env]; otherwise it
       is assumed by construction (CFill nodes reach here only after the
       typechecker has validated the binder's kind elsewhere). *)
    | CFill (x1, c1'), CFill (x2, c2') ->
      x1 = x2
      && (match List.assoc_opt x1 kind_env with
          | None | Some (KATC _) -> true
          | Some _ -> false)
      && cty_sub ~kind_env c1' c2'
    | _ -> false

let compose_eff = compose_effs

let sub_eff = eff_sub

(** The unique value type component of a computation type, if it has one. *)
let ty_of_cty (c: cty) : ty =
  match c with
  | CCty (t, _) -> t
  | _ -> typing_error "ty_of_cty: unexpected cty %s\n" (cty_to_str c)

let eff_of_cty (c: cty) : eff =
  match c with
  | CCty (_, e) -> e
  | _ -> typing_error "eff_of_cty: unexpected cty %s\n" (cty_to_str c)

let make_pure_cty (t: ty) : cty = CCty (t, EPure)


(** Type substitution **)

(** Generates a fresh label. *)
let fresh_label_var =
  let counter = ref 0 in
  fun () ->
    let var = "__l" ^ string_of_int !counter in
    incr counter;
    var

let rec rename_type_var ty var_old var_new =
  let rename_captured_set captured_set var_old var_new =
    let (cap_opt, labels) = captured_set in
    let labels' = Varset.map (fun var -> if var = var_old then var_new else var) labels in
    match cap_opt with
    | Some cap -> Some (if cap = var_old then var_new else cap), labels'
    | None -> None, labels'
  in
  match ty with
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let region' = match region with
      | RVar v when v = var_old -> RVar var_new
      | _ -> region
    in
    TFun {
      captured_set = rename_captured_set captured_set var_old var_new;
      cap_params = List.map (fun a -> if a = var_old then var_new else a) cap_params;
      label_params = List.map (fun (l, eff) -> if l = var_old then (var_new, eff) else (l, eff)) label_params;
      params_ty = List.map (fun (name, ty) -> (name, rename_type_var ty var_old var_new)) params_ty;
      region = region';
      return_cty = rename_type_var_cty return_cty var_old var_new
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set = rename_captured_set captured_set var_old var_new;
      effect_return_var;
      effect_return_ty = rename_type_var effect_return_ty var_old var_new;
      return_cty = rename_type_var_cty return_cty var_old var_new
    }
  | TRef ty' -> TRef (rename_type_var ty' var_old var_new)
  | TRefine (v, inner, p) ->
    (* Refinement predicates only mention term variables, not label/cap vars. *)
    TRefine (v, rename_type_var inner var_old var_new, p)
  | _ -> ty

and rename_type_var_cty c var_old var_new =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (rename_type_var t var_old var_new, rename_type_var_eff e var_old var_new)
  | CFill (v, c') -> CFill (v, rename_type_var_cty c' var_old var_new)

and rename_type_var_eff e var_old var_new =
  match e with
  | EPure -> EPure
  | EAns (x, c1, c2) ->
    EAns (x, rename_type_var_cty c1 var_old var_new, rename_type_var_cty c2 var_old var_new)
  | EEffVar _ -> e

(** Makes a single label substitution to the capture set. *)
let substitute_label_to_captured_set (captured_set: capability) label label_new =
  match captured_set with
  | cap, labels -> cap, Varset.map (fun l -> if l = label then label_new else l) labels

(** Makes a single capability substitution to the capture set. *)
let substitute_cap_to_captured_set (captured_set: capability) cap_var capability =
  match captured_set with
  | None, _ -> captured_set
  | Some cap, labels ->
    if cap <> cap_var then captured_set else
      match capability with
      | None, labels' -> None, (Varset.union labels labels')
      | Some cap', labels' -> Some cap', (Varset.union labels labels')

(** Makes a single label subsitution on the given type. *)
let rec substitute_label_to_type ty label label_new =
  match ty with
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let captured_set' = substitute_label_to_captured_set captured_set label label_new in
    (match List.assoc_opt label label_params with
    | Some _ -> TFun { captured_set = captured_set'; cap_params; label_params; params_ty; region; return_cty }
    | None ->
      let label_params', params_ty', return_cty' =
        if List.exists (fun (l, _) -> l = label_new) label_params
          then let fresh_label = (fresh_label_var()) in
            List.map (fun (l, eff) -> if l = label_new then (fresh_label, eff) else (l, eff)) label_params,
            List.map (fun (name, param_ty) -> (name, rename_type_var param_ty label_new fresh_label)) params_ty,
            rename_type_var_cty return_cty label_new fresh_label
          else label_params, params_ty, return_cty in
        TFun {
          captured_set = captured_set';
          cap_params;
          label_params = label_params';
          params_ty = List.map (fun (name, ty) -> (name, substitute_label_to_type ty label label_new)) params_ty';
          region;
          return_cty = substitute_label_to_cty return_cty' label label_new
        })
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    let captured_set' = substitute_label_to_captured_set captured_set label label_new in
    TCont {
      captured_set = captured_set';
      effect_return_var;
      effect_return_ty = substitute_label_to_type effect_return_ty label label_new;
      return_cty = substitute_label_to_cty return_cty label label_new
    }
  | TRef ty' -> TRef (substitute_label_to_type ty' label label_new)
  | TRefine (v, inner, p) ->
    TRefine (v, substitute_label_to_type inner label label_new, p)
  | _ -> ty

and substitute_label_to_cty c label label_new =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (substitute_label_to_type t label label_new, substitute_label_to_eff e label label_new)
  | CFill (v, c') -> CFill (v, substitute_label_to_cty c' label label_new)

and substitute_label_to_eff e label label_new =
  match e with
  | EPure -> EPure
  | EAns (x, c1, c2) -> EAns (x, substitute_label_to_cty c1 label label_new, substitute_label_to_cty c2 label label_new)
  | EEffVar _ -> e

(** Makes a single capability subsitution on the given type. *)
let rec substitute_capability_to_type ty cap_var capability =
  match ty with
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let captured_set' = substitute_cap_to_captured_set captured_set cap_var capability in
    if List.exists (fun a -> a = cap_var) cap_params
      then TFun { captured_set = captured_set'; cap_params; label_params; params_ty; region; return_cty }
      else
        let free_vars = (match capability with
        | (Some cap, labels) -> cap::(Varset.to_list labels)
        | (None, labels) -> Varset.to_list labels
        ) in
        let (label_params', params_ty', return_cty') = List.fold_right (fun label (label_vars, params_ty, return_cty) ->
            if List.exists (fun (l, _) -> l = label) label_vars
              then let fresh_label = (fresh_label_var()) in
                (List.map (fun (l, eff) -> if l = label then (fresh_label, eff) else (l, eff)) label_vars,
                List.map (fun (name, param_ty) -> (name, rename_type_var param_ty label fresh_label)) params_ty,
                rename_type_var_cty return_cty label fresh_label)
              else (label_vars, params_ty, return_cty)
          ) free_vars (label_params, params_ty, return_cty) in
          TFun {
            captured_set = captured_set';
            cap_params;
            label_params = label_params';
            params_ty = List.map (fun (name, ty) -> (name, substitute_capability_to_type ty cap_var capability)) params_ty';
            region;
            return_cty = substitute_capability_to_cty return_cty' cap_var capability
          }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    let captured_set' = substitute_cap_to_captured_set captured_set cap_var capability in
    TCont {
      captured_set = captured_set';
      effect_return_var;
      effect_return_ty = substitute_capability_to_type effect_return_ty cap_var capability;
      return_cty = substitute_capability_to_cty return_cty cap_var capability
    }
  | TRef ty' -> TRef (substitute_capability_to_type ty' cap_var capability)
  | TRefine (v, inner, p) ->
    TRefine (v, substitute_capability_to_type inner cap_var capability, p)
  | _ -> ty

and substitute_capability_to_cty c cap_var capability =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (substitute_capability_to_type t cap_var capability, substitute_capability_to_eff e cap_var capability)
  | CFill (v, c') -> CFill (v, substitute_capability_to_cty c' cap_var capability)

and substitute_capability_to_eff e cap_var capability =
  match e with
  | EPure -> EPure
  | EAns (x, c1, c2) -> EAns (x, substitute_capability_to_cty c1 cap_var capability, substitute_capability_to_cty c2 cap_var capability)
  | EEffVar _ -> e

(** Returns the result of applying the label and capability subsitutions to a given type. *)
let substitute_to_type ty label_subs cap_subs =
  let ty' = List.fold_right (fun (label, label_new) ty -> substitute_label_to_type ty label label_new) label_subs ty in
  List.fold_right(fun (cap_var, capability) ty -> substitute_capability_to_type ty cap_var capability) cap_subs ty'

let substitute_to_cty c label_subs cap_subs =
  let c' = List.fold_right (fun (label, label_new) c -> substitute_label_to_cty c label label_new) label_subs c in
  List.fold_right (fun (cap_var, capability) c -> substitute_capability_to_cty c cap_var capability) cap_subs c'

(** Returns the result of type subsitution **)
let substitute_ty ty type_subs =
  let fresh_var =
    let counter = ref 0 in
    fun () ->
      let var = "'__t" ^ (string_of_int !counter) in
      incr counter;
      var
  in
  let rename_region_var_in_constraint var_old var_new { rc_inner; rc_dist; rc_outer } =
    let rename_region = function
      | RVar v when v = var_old -> RVar var_new
      | r -> r
    in
    let rec rename_distance = function
      | DVar v when v = var_old -> DVar var_new
      | DPlus (d1, d2) -> DPlus (rename_distance d1, rename_distance d2)
      | d -> d
    in
    {
      rc_inner = rename_region rc_inner;
      rc_dist = rename_distance rc_dist;
      rc_outer = rename_region rc_outer;
    }
  in
  let rec substitute ty type_subs =
    match ty with
    | TRef ty' -> TRef (substitute ty' type_subs)
    | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
      let params_ty' = List.map (fun (name, ty) -> (name, substitute ty type_subs)) params_ty in
      let return_cty' = substitute_cty return_cty type_subs in
      TFun { captured_set; cap_params; label_params; params_ty=params_ty'; region; return_cty=return_cty' }
    | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
      let effect_return_ty' = substitute effect_return_ty type_subs in
      let return_cty' = substitute_cty return_cty type_subs in
      TCont { captured_set; effect_return_var; effect_return_ty=effect_return_ty'; return_cty=return_cty' }
    | TNode ty' -> TNode (substitute ty' type_subs)
    | TTree ty' -> TTree (substitute ty' type_subs)
    | TQueue ty' -> TQueue (substitute ty' type_subs)
    | TArray ty' -> TArray (substitute ty' type_subs)
    | TVar tv ->
      (match List.assoc_opt tv type_subs with
        | Some ty' -> ty'
        | None -> TVar tv
      )
    | TCon (con, t_args) ->
      let t_args' = List.map (fun t_arg -> substitute t_arg type_subs) t_args in
      TCon (con, t_args')
    | TForall (tv, kind, constraints, ty') ->
      let tv_new = fresh_var() in
      let type_subs' = (tv, TVar tv_new)::type_subs in
      TForall (
        tv_new,
        kind,
        List.map (rename_region_var_in_constraint tv tv_new) constraints,
        substitute ty' type_subs'
      )
    | TRefine (v, inner, p) ->
      TRefine (v, substitute inner type_subs, p)
    | _ -> ty
  and substitute_cty c type_subs =
    match c with
    | CTyVar _ -> c
    | CCty (t, e) -> CCty (substitute t type_subs, substitute_eff e type_subs)
    | CFill (v, c') -> CFill (v, substitute_cty c' type_subs)
  and substitute_eff e type_subs =
    match e with
    | EPure -> EPure
    | EAns (x, c1, c2) -> EAns (x, substitute_cty c1 type_subs, substitute_cty c2 type_subs)
    | EEffVar _ -> e
  in
  substitute ty type_subs

(** Substitute a term variable inside refinements nested in types/ctys. This
    is the value-level substitution needed by the Do rule's [{V/x}] premise. *)
let rec substitute_term_to_type ty var witness =
  match ty with
  | TRef ty' -> TRef (substitute_term_to_type ty' var witness)
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    let rec subst_params params =
      match params with
      | [] -> [], true
      | (name, t) :: rest ->
        let t' = substitute_term_to_type t var witness in
        (match name with
         | Some x when x = var ->
           (name, t') :: rest, false
         | _ ->
           let rest', subst_return = subst_params rest in
           (name, t') :: rest', subst_return)
    in
    let params_ty', subst_return = subst_params params_ty in
    TFun {
      captured_set; cap_params; label_params;
      params_ty = params_ty';
      region;
      return_cty =
        if subst_return then substitute_term_to_cty return_cty var witness
        else return_cty
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = substitute_term_to_type effect_return_ty var witness;
      return_cty =
        if effect_return_var = Some var
          then return_cty
          else substitute_term_to_cty return_cty var witness
    }
  | TNode t -> TNode (substitute_term_to_type t var witness)
  | TTree t -> TTree (substitute_term_to_type t var witness)
  | TQueue t -> TQueue (substitute_term_to_type t var witness)
  | TArray t -> TArray (substitute_term_to_type t var witness)
  | TCon (name, args) -> TCon (name, List.map (fun t -> substitute_term_to_type t var witness) args)
  | TForall (tv, kind, constraints, t) ->
    TForall (tv, kind, constraints, substitute_term_to_type t var witness)
  | TRefine (v, inner, p) when v = var ->
    TRefine (v, substitute_term_to_type inner var witness, p)
  | TRefine (v, inner, p) ->
    let inner' = substitute_term_to_type inner var witness in
    (match subst_expr_in_pred p var witness with
     | Some p' -> TRefine (v, inner', p')
     | None -> inner')
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty

and substitute_term_to_cty c var witness =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) -> CCty (substitute_term_to_type t var witness, substitute_term_to_eff e var witness)
  | CFill (x, c') -> CFill (x, substitute_term_to_cty c' var witness)

and substitute_term_to_eff e var witness =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (Some x, c1, c2) when x = var ->
    EAns (Some x, c1, substitute_term_to_cty c2 var witness)
  | EAns (x, c1, c2) ->
    EAns (x, substitute_term_to_cty c1 var witness, substitute_term_to_cty c2 var witness)

let pred_of_tylike = function
  | TLPred (params, body) -> Some (params, body)
  | _ -> None

let rec substitute_pred_in_term pred_name pred_params pred_body t =
  match t with
  | PTUnit | PTInt _ | PTBool _ | PTVar _ -> t
  | PTArith (t1, op, t2) ->
    PTArith (substitute_pred_in_term pred_name pred_params pred_body t1, op,
             substitute_pred_in_term pred_name pred_params pred_body t2)

and substitute_pred_in_pred pred_name pred_params pred_body p =
  match p with
  | PAtom t -> PAtom (substitute_pred_in_term pred_name pred_params pred_body t)
  | PCmp (t1, op, t2) ->
    PCmp (substitute_pred_in_term pred_name pred_params pred_body t1, op,
          substitute_pred_in_term pred_name pred_params pred_body t2)
  | PBArith (p1, op, p2) ->
    PBArith (substitute_pred_in_pred pred_name pred_params pred_body p1, op,
             substitute_pred_in_pred pred_name pred_params pred_body p2)
  | PNeg p' -> PNeg (substitute_pred_in_pred pred_name pred_params pred_body p')
  | PApp (p_name, args) when p_name = pred_name && List.length args = List.length pred_params ->
    let args' = List.map (substitute_pred_in_term pred_name pred_params pred_body) args in
    List.fold_left2
      (fun body (param, _) arg -> subst_var_in_pred body param arg)
      pred_body pred_params args'
  | PApp (p_name, args) ->
    PApp (p_name, List.map (substitute_pred_in_term pred_name pred_params pred_body) args)

let rec substitute_pred_to_type ty pred_name pred_params pred_body =
  match ty with
  | TRef ty' -> TRef (substitute_pred_to_type ty' pred_name pred_params pred_body)
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    TFun {
      captured_set; cap_params; label_params;
      params_ty = List.map (fun (name, t) -> (name, substitute_pred_to_type t pred_name pred_params pred_body)) params_ty;
      region;
      return_cty = substitute_pred_to_cty return_cty pred_name pred_params pred_body
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = substitute_pred_to_type effect_return_ty pred_name pred_params pred_body;
      return_cty = substitute_pred_to_cty return_cty pred_name pred_params pred_body
    }
  | TNode t -> TNode (substitute_pred_to_type t pred_name pred_params pred_body)
  | TTree t -> TTree (substitute_pred_to_type t pred_name pred_params pred_body)
  | TQueue t -> TQueue (substitute_pred_to_type t pred_name pred_params pred_body)
  | TArray t -> TArray (substitute_pred_to_type t pred_name pred_params pred_body)
  | TCon (name, args) ->
    TCon (name, List.map (fun t -> substitute_pred_to_type t pred_name pred_params pred_body) args)
  | TForall (tv, kind, constraints, body) when tv = pred_name ->
    TForall (tv, kind, constraints, body)
  | TForall (tv, kind, constraints, body) ->
    TForall (tv, kind, constraints, substitute_pred_to_type body pred_name pred_params pred_body)
  | TRefine (v, inner, p) ->
    TRefine (v,
             substitute_pred_to_type inner pred_name pred_params pred_body,
             substitute_pred_in_pred pred_name pred_params pred_body p)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty

and substitute_pred_to_cty c pred_name pred_params pred_body =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) ->
    CCty (substitute_pred_to_type t pred_name pred_params pred_body,
          substitute_pred_to_eff e pred_name pred_params pred_body)
  | CFill (v, c') -> CFill (v, substitute_pred_to_cty c' pred_name pred_params pred_body)

and substitute_pred_to_eff e pred_name pred_params pred_body =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (x, c1, c2) ->
    EAns (x,
          substitute_pred_to_cty c1 pred_name pred_params pred_body,
          substitute_pred_to_cty c2 pred_name pred_params pred_body)

(** Substitute a region-kinded type variable for a concrete region. *)
let rec substitute_region_to_type ty region_var (replacement: region) =
  let subst_region r =
    match r with
    | RVar v when v = region_var -> replacement
    | _ -> r
  in
  let rec subst_distance d =
    match d with
    | DVar v when v = region_var ->
      (* Region substitution does not provide a distance witness; keep
         distance variables unless a future surface syntax adds one. *)
      DVar v
    | DPlus (d1, d2) -> DPlus (subst_distance d1, subst_distance d2)
    | _ -> d
  in
  let substitute_region_to_constraint region_var replacement { rc_inner; rc_dist; rc_outer } =
    let subst_region r =
      match r with
      | RVar v when v = region_var -> replacement
      | _ -> r
    in
    {
      rc_inner = subst_region rc_inner;
      rc_dist = subst_distance rc_dist;
      rc_outer = subst_region rc_outer;
    }
  in
  match ty with
  | TRef t -> TRef (substitute_region_to_type t region_var replacement)
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    TFun {
      captured_set; cap_params; label_params;
      params_ty = List.map (fun (name, t) -> (name, substitute_region_to_type t region_var replacement)) params_ty;
      region = subst_region region;
      return_cty = substitute_region_to_cty return_cty region_var replacement
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = substitute_region_to_type effect_return_ty region_var replacement;
      return_cty = substitute_region_to_cty return_cty region_var replacement
    }
  | TNode t -> TNode (substitute_region_to_type t region_var replacement)
  | TTree t -> TTree (substitute_region_to_type t region_var replacement)
  | TQueue t -> TQueue (substitute_region_to_type t region_var replacement)
  | TArray t -> TArray (substitute_region_to_type t region_var replacement)
  | TCon (name, args) ->
    TCon (name, List.map (fun t -> substitute_region_to_type t region_var replacement) args)
  | TForall (tv, kind, constraints, body) when tv = region_var ->
    TForall (tv, kind, constraints, body)
  | TForall (tv, kind, constraints, body) ->
    TForall (
      tv,
      kind,
      List.map (substitute_region_to_constraint region_var replacement) constraints,
      substitute_region_to_type body region_var replacement
    )
  | TRefine (v, inner, p) ->
    TRefine (v, substitute_region_to_type inner region_var replacement, p)
  | TCap (r, opty) -> TCap (subst_region r, opty)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ -> ty

and substitute_region_to_cty c region_var replacement =
  match c with
  | CTyVar _ -> c
  | CCty (t, e) ->
    CCty (substitute_region_to_type t region_var replacement,
          substitute_region_to_eff e region_var replacement)
  | CFill (v, c') -> CFill (v, substitute_region_to_cty c' region_var replacement)

and substitute_region_to_eff e region_var replacement =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (x, c1, c2) ->
    EAns (x,
          substitute_region_to_cty c1 region_var replacement,
          substitute_region_to_cty c2 region_var replacement)

let rec substitute_cty_var_to_type ty cty_var replacement =
  match ty with
  | TRef t -> TRef (substitute_cty_var_to_type t cty_var replacement)
  | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
    TFun {
      captured_set; cap_params; label_params;
      params_ty = List.map (fun (name, t) -> (name, substitute_cty_var_to_type t cty_var replacement)) params_ty;
      region;
      return_cty = substitute_cty_var_to_cty return_cty cty_var replacement
    }
  | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
    TCont {
      captured_set;
      effect_return_var;
      effect_return_ty = substitute_cty_var_to_type effect_return_ty cty_var replacement;
      return_cty = substitute_cty_var_to_cty return_cty cty_var replacement
    }
  | TNode t -> TNode (substitute_cty_var_to_type t cty_var replacement)
  | TTree t -> TTree (substitute_cty_var_to_type t cty_var replacement)
  | TQueue t -> TQueue (substitute_cty_var_to_type t cty_var replacement)
  | TArray t -> TArray (substitute_cty_var_to_type t cty_var replacement)
  | TCon (name, args) ->
    TCon (name, List.map (fun t -> substitute_cty_var_to_type t cty_var replacement) args)
  | TForall (tv, kind, constraints, body) when tv = cty_var ->
    TForall (tv, kind, constraints, body)
  | TForall (tv, kind, constraints, body) ->
    TForall (tv, kind, constraints, substitute_cty_var_to_type body cty_var replacement)
  | TRefine (v, inner, p) ->
    TRefine (v, substitute_cty_var_to_type inner cty_var replacement, p)
  | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty

and substitute_cty_var_to_cty c cty_var replacement =
  match c with
  | CTyVar v when v = cty_var -> replacement
  | CTyVar _ -> c
  | CCty (t, e) ->
    CCty (substitute_cty_var_to_type t cty_var replacement,
          substitute_cty_var_to_eff e cty_var replacement)
  | CFill (v, c') -> CFill (v, substitute_cty_var_to_cty c' cty_var replacement)

and substitute_cty_var_to_eff e cty_var replacement =
  match e with
  | EPure | EEffVar _ -> e
  | EAns (x, c1, c2) ->
    EAns (x,
          substitute_cty_var_to_cty c1 cty_var replacement,
          substitute_cty_var_to_cty c2 cty_var replacement)

(* A region tylike argument can be written bare in surface syntax: [[top]],
   a VAR (a region binder like [[r1]]), or a TYPE_VAR (a region-kinded
   abstract var like [['rho]]). The first parses as [TLRegion RTop]; the
   other two parse as [TLTy] under the default [type_exp] production and
   are reinterpreted here. *)
let region_of_tylike = function
  | TLRegion r -> Some r
  | TLTy (TCon (name, [])) -> Some (RVar name)
  | TLTy (TVar tv) -> Some (RVar tv)
  | _ -> None

let substitute_tylike_to_type ty var kind arg =
  match kind, arg with
  | KTy, TLTy replacement -> substitute_ty ty [(var, replacement)]
  | KPred _, _ ->
    (match pred_of_tylike arg with
     | Some (params, body) -> substitute_pred_to_type ty var params body
     | None -> ty)
  | KCty, TLCty c -> substitute_cty_var_to_type ty var c
  | KCty, TLTy t -> substitute_cty_var_to_type ty var (CCty (t, EPure))
  | KReg, _ ->
    (match region_of_tylike arg with
     | Some r -> substitute_region_to_type ty var r
     | None -> ty)
  | _ -> ty

let substitute_tylike_to_cty c var kind arg =
  match kind, arg with
  | KTy, TLTy replacement ->
    let rec subst_c c = match c with
      | CTyVar _ -> c
      | CCty (t, e) -> CCty (substitute_ty t [(var, replacement)], subst_e e)
      | CFill (x, c') -> CFill (x, subst_c c')
    and subst_e e = match e with
      | EPure | EEffVar _ -> e
      | EAns (x, c1, c2) -> EAns (x, subst_c c1, subst_c c2)
    in subst_c c
  | KPred _, _ ->
    (match pred_of_tylike arg with
     | Some (params, body) -> substitute_pred_to_cty c var params body
     | None -> c)
  | KCty, TLCty replacement -> substitute_cty_var_to_cty c var replacement
  | KCty, TLTy t -> substitute_cty_var_to_cty c var (CCty (t, EPure))
  | KReg, _ ->
    (match region_of_tylike arg with
     | Some r -> substitute_region_to_cty c var r
     | None -> c)
  | _ -> c

let substitute_tylikes_to_type ty bindings args =
  List.fold_left2
    (fun acc (var, kind) arg -> substitute_tylike_to_type acc var kind arg)
    ty bindings args

let substitute_tylikes_to_cty c bindings args =
  List.fold_left2
    (fun acc (var, kind) arg -> substitute_tylike_to_cty acc var kind arg)
    c bindings args

let promote_cty_vars_in_type kind_env ty =
  let is_cty_var v =
    match List.assoc_opt v kind_env with
    | Some KCty -> true
    | _ -> false
  in
  let rec go_ty ty =
    match ty with
    | TRef t -> TRef (go_ty t)
    | TFun { captured_set; cap_params; label_params; params_ty; region; return_cty } ->
      TFun {
        captured_set;
        cap_params;
        label_params;
        params_ty = List.map (fun (name, t) -> (name, go_ty t)) params_ty;
        region;
        return_cty = go_cty return_cty
      }
    | TCont { captured_set; effect_return_var; effect_return_ty; return_cty } ->
      TCont {
        captured_set;
        effect_return_var;
        effect_return_ty = go_ty effect_return_ty;
        return_cty = go_cty return_cty
      }
    | TNode t -> TNode (go_ty t)
    | TTree t -> TTree (go_ty t)
    | TQueue t -> TQueue (go_ty t)
    | TArray t -> TArray (go_ty t)
    | TCon (name, args) -> TCon (name, List.map go_ty args)
    | TForall (tv, kind, constraints, body) ->
      TForall (tv, kind, constraints, go_ty body)
    | TRefine (v, inner, p) -> TRefine (v, go_ty inner, p)
    | TUnit | TInt | TBool | TFloat | TChar | TStr | TVar _ | TCap _ -> ty
  and go_cty c =
    match c with
    | CCty (TVar v, EPure) when is_cty_var v -> CTyVar v
    | CTyVar _ -> c
    | CCty (t, e) -> CCty (go_ty t, go_eff e)
    | CFill (v, c') -> CFill (v, go_cty c')
  and go_eff e =
    match e with
    | EPure | EEffVar _ -> e
    | EAns (x, c1, c2) -> EAns (x, go_cty c1, go_cty c2)
  in
  go_ty ty

let promote_cty_vars_in_cty kind_env c =
  match promote_cty_vars_in_type kind_env (TFun {
    captured_set = None, Varset.empty;
    cap_params = [];
    label_params = [];
    params_ty = [];
    region = RTop;
    return_cty = c;
  }) with
  | TFun { return_cty; _ } -> return_cty
  | _ -> c
