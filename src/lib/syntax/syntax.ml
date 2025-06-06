open Common

type top_level =
  | TLAbs of var * var list * expr
  | TLBody of var * var list * expr
  | TLEffSig of var * var list
  | TLEffZSig of var * var list
  | TLType of typedef list
  | TLOpen of var
  | TLOpenC of var

and fundef = { name : var;
               params : var list;
               body : expr }

and hdl = { op_anno : hdl_anno;
            op_name : var;
            op_params : var list;
            op_body : expr }

and expr =
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
  | App of expr * expr list * metadata
  | New of expr list
  | Get of expr * expr
  | Set of expr * expr * expr
  | Raise of {
    raise_stub : expr;
    raise_op : var;
    raise_args : expr list
  }
  | RaiseZ of {
    clue_sig : var;
    clue_type: int;
    clue_label : int;
    raisez_op : var;
    raisez_args : expr list
  }
  | Resume of expr * expr * capability
  | ResumeFinal of expr * expr * capability
  | Handle of { handle_body : expr;
    stub : var;
    sig_name : var;
    handler_defs : hdl list;
    captured_set : capability
  }
  | HandleZ of {
    handle_body : expr;
    sig_name : var;
    handler_defs : hdl list;
    captured_set : capability
  }
  | Recdef of fundef list * expr
  | Fun of var list * expr
  | Let of var * expr * expr
  | If of expr * expr * expr
  | Stmt of expr * expr
  | Typecon of var * expr list
  | Match of {
    match_expr : expr;
    pattern_matching : (pattern * expr) list
  }