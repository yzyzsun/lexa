open Syntax__Common
open Syntax__Varset

type top_level =
  | TLPolyAbs of var * (var list) * (var list) * (var * var) list * parameter list * ty * expr (* name ; type params; cap vars ; labels ; params ; body *)
  | TLAbs of var * (var list) * (var * var) list * parameter list * ty * expr (* name ; cap vars ; labels ; params ; body *)
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
               return_ty : ty}

and hdl = { op_anno : hdl_anno;
            op_name : var;
            op_params : var list;
            op_body : expr }

and return_clause = {
  return_var : var;
  return_body : expr;
}

and capability = var option * Varset.t

and capture_set = 
  | Capability of string
  | Labels of (string * string) list

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
  | Raise of {
    raise_label : var;
    raise_op : var;
    raise_args : expr list
  }
  | Resume of expr * expr
  | ResumeFinal of expr * expr
  | Handle of { 
    captured_set : capability;
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
    return_ty: ty
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
    return_ty : ty
  }
  | TCont of {
    captured_set : capability;
    effect_return_ty : ty;
    return_ty : ty
  }
  | TNode of ty (* node_t* *)
  | TTree of ty (* tree_t* *)
  | TQueue of ty (* queue_t* *)
  | TArray of ty (* array_t* *)
  | TCon of var * ty list
  | TVar of var
  | TForall of var * ty

and parameter = var * ty

