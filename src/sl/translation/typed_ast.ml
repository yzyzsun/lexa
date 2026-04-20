open SLsyntax
open Syntax__Common

type typed_expr =
{
  expr_desc: typed_expr_desc;
  expr_cty: cty;
  captured_vars: capture_set;
  cap_vars: var list;
  label_vars: (var * label_binding) list;
}

and fundef = { name : var;
               captured_set : SLsyntax.capability;
               cap_params : var list;
               label_params : (var * var) list;
               params : parameter list;
               body : typed_expr;
               return_cty : cty}

and hdl = { op_anno : hdl_anno;
            op_name : var;
            op_params : var list;
            op_body : typed_expr }

and typed_return_clause = {
  return_var : var;
  return_var_ty : ty;
  (* The inferred output cty of the return-clause body (C1). *)
  return_cty : cty;
  return_body : typed_expr;
}

and typed_expr_desc = 
  | Unit
  | Var of var
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Char of char
  | Prim of string
  | Arith of typed_expr * arith * typed_expr
  | Cmp of typed_expr * cmp * typed_expr
  | Neg of typed_expr
  | BArith of typed_expr * barith * typed_expr
  | App of {
    func: typed_expr;
    cap_insts: SLsyntax.capability list;
    label_args: var list;
    args: typed_expr list
  }
  | New of typed_expr list
  | Get of (typed_expr * typed_expr)
  | Set of typed_expr * typed_expr * typed_expr
  | Do of {
    do_label : var;
    do_op : var;
    do_evidence : SLsyntax.evidence;
    do_typelike_args : SLsyntax.typelike list;
    do_args : typed_expr list;
  }
  | Resume of (typed_expr * typed_expr)
  | ResumeFinal of (typed_expr * typed_expr)
  | Handle of {
    captured_set : SLsyntax.capability;
    region_binder : var;
    evidence_binder : var;
    handle_body : typed_expr;
    handler_label : var;
    sig_name : var;
    return_clause : typed_return_clause option;
    handler_defs : hdl list;
  }
  | Recdef of (fundef list * typed_expr)
  | Fun of {
    captured_set: SLsyntax.capability;
    cap_params: var list;
    label_params: (var * var) list;
    params: parameter list;
    body: typed_expr;
    return_cty: cty
  }
  | Let of (var * typed_expr * typed_expr)
  | If of (typed_expr * typed_expr * typed_expr)
  | Stmt of (typed_expr * typed_expr)
  | Typecon of (var * ty list * typed_expr list)
  | Match of {
    match_expr : typed_expr;
    pattern_matching : (pattern * typed_expr) list
  }
  | TypeApp of ty * typed_expr