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

(** Finds the effect name associated to the label. *)
let find_effect_name_opt label captured_vars label_vars =
  match List.assoc_opt label label_vars with
  | Some eff_name -> Some eff_name
  | None ->
    match captured_vars with
    | Labels captured_labels -> List.assoc_opt label captured_labels 
    | _ -> None

(** Finds the effect name associated to the label. Raises an exception if the label doesn't exist. *)
let find_effect_name label captured_vars label_vars =
  match find_effect_name_opt label captured_vars label_vars with
  | Some eff_name -> eff_name
  | None -> typing_error "Label %s not found\n" label

(** Returns a formatted string for the given type. For debugging purpose. *)
let rec type_to_str (ty: ty) = 
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
  | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
    let captured_set_str = capability_to_str captured_set in
    let cap_params_str = String.concat ", " cap_params in
    let label_vars_str = String.concat ", " (List.map (fun (label, eff) -> Printf.sprintf "%s: %s" label eff) label_params) in
    let params_ty_str = String.concat ", " (List.map type_to_str params_ty) in
    let return_ty_str = type_to_str return_ty in
    Printf.sprintf "{ %s } [%s; %s] (%s) -> %s" captured_set_str cap_params_str label_vars_str params_ty_str return_ty_str
  | TCont { captured_set; effect_return_ty; return_ty } ->
    let captured_set_str = capability_to_str captured_set in
    let effect_ty_str = type_to_str effect_return_ty in
    let return_ty_str = type_to_str return_ty in
    Printf.sprintf "{ %s } cont %s -> %s" captured_set_str effect_ty_str return_ty_str
  | TNode t -> Printf.sprintf "node_t::[%s]" (type_to_str t)
  | TTree t -> Printf.sprintf "tree_t::[%s]" (type_to_str t)
  | TQueue t -> Printf.sprintf "queue_t::[%s]" (type_to_str t)
  | TArray t -> Printf.sprintf "array_t::[%s]" (type_to_str t)
  | TCon (t, t_args) -> 
    if (t_args = [])
      then t else Printf.sprintf "%s::[%s]" t (String.concat ", " (List.map type_to_str t_args))
  | TVar v -> v
  | TForall (tvar, _kind, ty') -> Printf.sprintf "∀%s. %s" tvar (type_to_str ty')
  | TCap (_region, _opty) -> "Cap"

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

  let rec normalize ty env =
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

    match ty with
    | TRef ty' -> TRef (normalize ty' env)
    | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
      let captured_set' = rename_captured_set captured_set env in
      let new_caps = List.map (fun _ -> fresh_label()) cap_params in
      let new_labels = List.map (fun (_, eff) -> (fresh_label(), eff)) label_params in
      let env' = List.fold_right2 (fun label label_new env ->
        Varmap.add label label_new env
      ) (cap_params@(List.map fst label_params)) (new_caps@(List.map fst new_labels)) env in

      TFun { 
        captured_set = captured_set'; 
        cap_params = new_caps; 
        label_params = new_labels;
        params_ty = List.map (fun ty -> normalize ty env') params_ty;
        return_ty = normalize return_ty env'
      }
    | TCont { captured_set; effect_return_ty; return_ty } ->
      TCont {
        captured_set = rename_captured_set captured_set env;
        effect_return_ty = normalize effect_return_ty env;
        return_ty = normalize return_ty env
      }
    | TCon (tv, t_args) ->
      let t_args' = List.map (fun t_arg -> normalize t_arg env) t_args in
      TCon (tv, t_args')
    | TVar tv -> (match Varmap.find_opt tv env with
      | Some tv' -> TVar tv'
      | None -> TVar tv
    )
    | TForall (tv, kind, ty') ->
      let tv_new = fresh_var() in
      let env' = Varmap.add tv tv_new env in
      TForall (tv_new, kind, (normalize ty' env'))
    | _ -> ty
  
  in 
  let result = normalize ty Varmap.empty in
  result

(** Checks for equality of two types. *)
let types_eq t1 t2 = 
  let captured_sets_eq cs1 cs2 = 
    match cs1, cs2 with
    | (None, labels1), (None, labels2) -> Varset.equal labels1 labels2
    | (Some cap1, labels1), (Some cap2, labels2) -> (cap1 = cap2) && (Varset.equal labels1 labels2)
    | _ -> false
  in

  let rec normalized_types_eq t1 t2 = 
    match (t1, t2) with
    | (TRef t1', TRef t2') -> normalized_types_eq t1' t2'
    | (TFun {captured_set = cs; cap_params = cp; label_params=lp; params_ty = pt; return_ty = rt}, 
      TFun {captured_set = cs'; cap_params = cp'; label_params = lp'; params_ty = pt' ; return_ty = rt'}) ->
        (captured_sets_eq cs cs') 
        && (cp = cp')
        && (lp = lp')
        && (List.equal normalized_types_eq pt pt')
        && (normalized_types_eq rt rt')
    | (TCont { captured_set = cs; effect_return_ty = et; return_ty = rt },
      TCont { captured_set = cs'; effect_return_ty = et'; return_ty = rt' }) ->
        (captured_sets_eq cs cs') 
        && (normalized_types_eq et et')
        && (normalized_types_eq rt rt')
    | (TCon (tv1, t1_args), TCon (tv2, t2_args)) ->
      (tv1 = tv2) && (List.equal normalized_types_eq t1_args t2_args)
    | (TForall (v1, k1, t1'), TForall (v2, k2, t2')) -> 
      v1 = v2 && k1 = k2 && (normalized_types_eq t1' t2')
    | (t1', t2') -> t1' = t2'
  in

  let t1_normalized = alpha_normalize t1 in
  let t2_normalized = alpha_normalize t2 in
  normalized_types_eq t1_normalized t2_normalized


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
  | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
    TFun {
      captured_set = rename_captured_set captured_set var_old var_new;
      cap_params = List.map (fun a -> if a = var_old then var_new else a) cap_params;
      label_params = List.map (fun (l, eff) -> if l = var_old then (var_new, eff) else (l, eff)) label_params;
      params_ty = List.map (fun ty -> rename_type_var ty var_old var_new) params_ty;
      return_ty = rename_type_var return_ty var_old var_new
    }
  | TCont { captured_set; effect_return_ty; return_ty } ->
    TCont {
      captured_set = rename_captured_set captured_set var_old var_new;
      effect_return_ty = rename_type_var effect_return_ty var_old var_new;
      return_ty = rename_type_var return_ty var_old var_new
    }
  | TRef ty' -> TRef (rename_type_var ty' var_old var_new)
  | _ -> ty

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
  | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
    let captured_set' = substitute_label_to_captured_set captured_set label label_new in
    (match List.assoc_opt label label_params with
    | Some _ -> TFun { captured_set = captured_set'; cap_params; label_params; params_ty; return_ty }
    | None -> 
      (* If the new label is one of the label parameters, change the parameter to a fresh label to avoid capturing *)
      let label_params', params_ty', return_ty' = 
        if List.exists (fun (l, _) -> l = label_new) label_params
          then let fresh_label = (fresh_label_var()) in
            List.map (fun (l, eff) -> if l = label_new then (fresh_label, eff) else (l, eff)) label_params,
            List.map (fun param_ty -> rename_type_var param_ty label_new fresh_label) params_ty,
            rename_type_var return_ty label_new fresh_label
          else label_params, params_ty, return_ty in
        TFun {
          captured_set = captured_set';
          cap_params;
          label_params = label_params';
          params_ty = List.map (fun ty -> substitute_label_to_type ty label label_new) params_ty';
          return_ty = substitute_label_to_type return_ty' label label_new
        })
  | TCont { captured_set; effect_return_ty; return_ty } ->
    let captured_set' = substitute_label_to_captured_set captured_set label label_new in
    TCont {
      captured_set = captured_set';
      effect_return_ty = substitute_label_to_type effect_return_ty label label_new;
      return_ty = substitute_label_to_type return_ty label label_new
    }
  | TRef ty' -> TRef (substitute_label_to_type ty' label label_new)
  | _ -> ty

(** Makes a single capability subsitution on the given type. *)
let rec substitute_capability_to_type ty cap_var capability =
  match ty with
  | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
    let captured_set' = substitute_cap_to_captured_set captured_set cap_var capability in
    if List.exists (fun a -> a = cap_var) cap_params 
      then TFun { captured_set = captured_set'; cap_params; label_params; params_ty; return_ty }
      else 
        let free_vars = (match capability with
        | (Some cap, labels) -> cap::(Varset.to_list labels)
        | (None, labels) -> Varset.to_list labels
        ) in
        let (label_params', params_ty', return_ty') = List.fold_right (fun label (label_vars, params_ty, return_ty) ->
            if List.exists (fun (l, _) -> l = label) label_vars
              then let fresh_label = (fresh_label_var()) in
                (List.map (fun (l, eff) -> if l = label then (fresh_label, eff) else (l, eff)) label_vars,
                List.map (fun param_ty -> rename_type_var param_ty label fresh_label) params_ty,
                rename_type_var return_ty label fresh_label)
              else (label_vars, params_ty, return_ty)
          ) free_vars (label_params, params_ty, return_ty) in
          TFun {
            captured_set = captured_set';
            cap_params;
            label_params = label_params';
            params_ty = List.map (fun ty -> substitute_capability_to_type ty cap_var capability) params_ty';
            return_ty = substitute_capability_to_type return_ty' cap_var capability
          }
  | TCont { captured_set; effect_return_ty; return_ty } ->
    let captured_set' = substitute_cap_to_captured_set captured_set cap_var capability in
    TCont {
      captured_set = captured_set';
      effect_return_ty = substitute_capability_to_type effect_return_ty cap_var capability;
      return_ty = substitute_capability_to_type return_ty cap_var capability
    }
  | TRef ty' -> TRef (substitute_capability_to_type ty' cap_var capability)
  | _ -> ty

(** Returns the result of applying the label and capability subsitutions to a given type. *)
let substitute_to_type ty label_subs cap_subs =
  let ty' = List.fold_right (fun (label, label_new) ty -> substitute_label_to_type ty label label_new) label_subs ty in
  List.fold_right(fun (cap_var, capability) ty -> substitute_capability_to_type ty cap_var capability) cap_subs ty'

(** Returns the result of type subsitution **)
let substitute_ty ty type_subs =
  let fresh_var = 
    let counter = ref 0 in
    fun () ->
      let var = "'__t" ^ (string_of_int !counter) in
      incr counter;
      var
  in

  let rec substitute ty type_subs =
    match ty with
    | TRef ty' -> TRef (substitute ty' type_subs)
    | TFun { captured_set; cap_params; label_params; params_ty; return_ty } ->
      let params_ty' = List.map (fun ty -> substitute ty type_subs) params_ty in
      let return_ty' = substitute return_ty type_subs in
      TFun { captured_set; cap_params; label_params; params_ty=params_ty'; return_ty=return_ty' }
    | TCont { captured_set; effect_return_ty; return_ty } ->
      let effect_return_ty' = substitute effect_return_ty type_subs in
      let return_ty' = substitute return_ty type_subs in
      TCont { captured_set; effect_return_ty=effect_return_ty'; return_ty=return_ty' }
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
    | TForall (tv, kind, ty') ->
      let tv_new = fresh_var() in
      let type_subs' = (tv, TVar tv_new)::type_subs in
      TForall (tv_new, kind, substitute ty' type_subs')
    | _ -> ty in
  
  substitute ty type_subs