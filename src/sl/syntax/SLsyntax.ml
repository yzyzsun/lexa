open Syntax__Common
open Syntax__Varset

type top_level =
  | TLPolyAbs of var * (var list) * (var list) * (var * var) list * parameter list * cty * expr (* name ; type params; cap vars ; labels ; params ; body *)
  | TLAbs of var * (var list) * (var * var) list * parameter list * cty * expr (* name ; cap vars ; labels ; params ; body *)
  | TLEffSig of var * (var * (ty list * ty)) list
  | TLEffZSig of var * (var * (ty list * ty)) list
  | TLType of typedef list
  | TLOpen of var
  | TLOpenC of var

and typedef = {
  type_name : var;
  type_params: var list;
  type_cons : (var * ty list) list
}

and fundef = { name : var;
               captured_set : capability;
               cap_params : var list;
               label_params : (var * var) list;
               params : parameter list;
               body : expr;
               return_cty : cty}

and hdl = { op_anno : hdl_anno;
            op_name : var;
            op_params : var list;
            op_body : expr }

and return_clause = {
  return_var : var;
  return_var_ty : ty;
  return_body : expr;
}

and capability = var option * Varset.t

and capture_set =
  | Capability of string
  | Labels of (string * label_binding) list

(* A label_binding records what a handler introduced for a label:
   the effect name it handles and, for each operation, the type the operation
   has under this handler (including the inferred ATM C1 => C2). *)
and label_binding = {
  lb_effect_name : var;
  lb_op_ctys : (var * op_cty) list;
}

and op_cty = {
  op_params_ty : ty list;
  op_return_ty : ty;
  op_c1 : cty;
  op_c2 : cty;
}

and expr =
  | Unit
  | Var of var
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Char of char
  | Prim of string
  | Arith of expr * arith * expr
  | Cmp of expr * cmp * expr 
  | Neg of expr
  | BArith of expr * barith * expr
  | App of {
    func: expr;
    cap_insts: capability list;
    label_args: var list;
    args: expr list
  }
  | New of expr list
  | Get of expr * expr
  | Set of expr * expr * expr
  | Do of {
    do_label : var;
    do_op : var;
    do_evidence : evidence;
    do_typelike_args : typelike list;
    do_args : expr list
  }
  | Resume of expr * expr
  | ResumeFinal of expr * expr
  | Handle of {
    captured_set : capability;
    region_binder : var;
    evidence_binder : var;
    handle_body : expr;
    handler_label : var;
    sig_name : var;
    return_clause : return_clause option;
    handler_defs : hdl list
  }
  | Recdef of fundef list * expr
  | Fun of {
    captured_set: capability;
    cap_params: var list;
    label_params: (var * var) list;
    params: parameter list;
    body: expr;
    return_cty: cty
  }
  | Let of var * expr * expr
  | If of expr * expr * expr
  | Stmt of expr * expr
  | Typecon of var * ty list * expr list
  | Match of {
    match_expr : expr;
    pattern_matching : (pattern * expr) list
  }
  | TypeApp of ty * expr

and ty = (* Lexaz SL types *)
  | TUnit
  | TInt
  | TBool
  | TFloat
  | TChar
  | TStr
  | TRef of ty
  | TFun of {
    captured_set : capability;
    cap_params : var list;
    label_params : (var * var) list;
    params_ty : ty list;
    return_cty : cty
  }
  | TCont of {
    captured_set : capability;
    effect_return_ty : ty;
    return_cty : cty
  }
  | TNode of ty (* node_t* *)
  | TTree of ty (* tree_t* *)
  | TQueue of ty (* queue_t* *)
  | TArray of ty (* array_t* *)
  | TCon of var * ty list
  | TVar of var
  | TForall of var * kind * ty
  | TCap of region * opty

and opty = {
  op_ty_bindings : (var * kind) list;
  op_param_name  : var;
  op_param_ty    : ty;
  op_return_cty  : cty;
}

and region =
  | RTop
  | RVar of var
  | RNull

and distance =
  | DZero
  | DOne
  | DVar of var
  | DPlus of distance * distance

and region_constraint = {
  rc_left  : region;
  rc_dist  : distance;
  rc_right : region;
}

and evidence =
  | EVar of var
  | EZero
  | EPlus of evidence * evidence
  | ENull

and kind =
  | KTy
  | KCty
  | KEff
  | KReg
  | KDist
  | KATC of distance
  | KEv of region_constraint
  | KPred of base_ty list

and base_ty =
  | BBool
  | BUnit
  | BInt

and atc =
  | ATCHole
  | ATCVar of var
  | ATCAns of ty * var * cty * atc
  | ATCFill of var * atc

and eff =
  | EPure
  | EAns of cty * cty
  | EEffVar of var

and cty =
  | CTyVar of var
  | CCty of ty * eff
  | CFill of var * cty

and typelike =
  | TLTy of ty
  | TLRegion of region
  | TLDist of distance
  | TLEvidence of evidence
  | TLCty of cty
  | TLEff of eff
  | TLATC of atc

and parameter = var * ty

