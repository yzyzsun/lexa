%{
  open SLsyntax
  open Syntax__Common
  open Syntax__Varset
  open Stdlib
%}
// %token NEWLINE

%token EOF

%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token <string> TYPE_VAR
// %token <string> SIG
%token <string> PRIM 

%token DOT
%token LPAREN
%token RPAREN
%token COMMA
%token LTS (* less than sign *)
%token GTS
%token EQ
%token COLONEQ
%token COLON
%token RAISE
%token HANDLE
%token LSB
%token RSB
%token WITH
%token NEWREF
%token DEF
%token TRUE
%token FALSE
%token LCB
%token RCB
%token ADD
%token SUB
%token MULT
%token DIV
%token NEQ
%token CMPEQ
%token LEQ
%token GEQ
%token IF
%token THEN
%token ELSE
%token EFFECT
%token EXC
%token HDL1
%token HDLS
%token RESUME
%token RESUMEFINAL
%token PERC
%token VALDEF
%token SEMICOLON
%token FUN
%token REC
%token AND
%token RETURN
%token TYPE
%token VBAR
%token OF
%token MATCH
%token RARROW
%token FATARROW
%token <string> STRING
%token <string> CAPITALIZED_VAR
%token <char> CHAR
%token OPEN
%token CONJ
%token DISJ
%token NEG
%token OPEN_C_HEADER

%token TUNIT
%token TINT
%token TFLOAT
%token TBOOL
%token TSTRING
%token TCHAR
%token TREF
%token TCONT
%token TNODE
%token TTREE
%token TQUEUE
%token TARRAY
%token FORALL
%token ATC

%token EXCEPTION

%start <SLsyntax.top_level list> prog

%nonassoc STMT
%nonassoc HIGHER_THAN_STMT
%left SEMICOLON
%nonassoc ELSE
%nonassoc COLONEQ
%left CONJ
%left DISJ 
%right NEG
%nonassoc NEQ CMPEQ LTS GTS LEQ GEQ
%left ADD SUB
%nonassoc CTY_PURE
%left MULT DIV PERC
%%

// There is probably more undiscovered bugs in the parsing rules

prog:
  | list(top_level); EOF { $1 }

type_con:
  | con_name = CAPITALIZED_VAR { (con_name, []) }
  | con_name = CAPITALIZED_VAR OF con_args = separated_nonempty_list(MULT, type_exp) { (con_name, con_args) }

type_def:
  | type_name = VAR EQ option(VBAR) type_cons = separated_nonempty_list(VBAR, type_con) { {type_name; type_params=[]; type_cons}: SLsyntax.typedef }
  | type_name = VAR COLON COLON
    LSB type_params = separated_list(COMMA, TYPE_VAR) RSB
    EQ option(VBAR) type_cons = separated_nonempty_list(VBAR, type_con) { {type_name; type_params; type_cons}: SLsyntax.typedef }

label_effect_pair:
  | label = VAR COLON eff = CAPITALIZED_VAR { (label, eff) }

capability: 
  | cap_var = VAR SEMICOLON inst_vars = separated_list(COMMA, VAR) { (Some cap_var, (Varset.of_list inst_vars)) }
  | inst_vars = separated_list(COMMA, VAR) { (None, (Varset.of_list inst_vars)) }

cap_inst:
  | LPAREN cap = capability RPAREN { cap }

opt_params:
  | LSB cap_params = separated_list(COMMA, VAR) SEMICOLON label_params = separated_list(COMMA, label_effect_pair) RSB
    { (cap_params, label_params) }
  | { ([], []) }

opt_args:
  | COLON LSB cap_insts = separated_list(COMMA, cap_inst) SEMICOLON label_args = separated_list(COMMA, VAR) RSB
    { (cap_insts, label_args) }
  | { ([], []) }

type_exp:
  | TUNIT { TUnit }
  | TINT { TInt }
  | TFLOAT { TFloat }
  | TBOOL { TBool }
  | TSTRING { TStr }
  | TCHAR { TChar }
  | TREF ty = type_exp { TRef ty }
  | LTS captured_set = capability GTS
    opt_params = opt_params
    LPAREN params_ty = separated_list(COMMA, type_exp) RPAREN RARROW return_cty = cty_exp
    { let (cap_params, label_params) = opt_params in
      TFun { captured_set; cap_params; label_params; params_ty; return_cty } }
  | TCONT LTS captured_set = capability GTS effect_return_ty = type_exp RARROW return_cty = cty_exp
    { TCont { captured_set; effect_return_ty; return_cty } }
  | TNODE COLON COLON LSB ty = type_exp RSB { TNode ty }
  | TTREE COLON COLON LSB ty = type_exp RSB { TTree ty }
  | TQUEUE COLON COLON LSB ty = type_exp RSB { TQueue ty }
  | TARRAY COLON COLON LSB ty = type_exp RSB { TArray ty }
  | pattern_name = VAR { TCon (pattern_name, []) }
  | pattern_name = VAR COLON COLON LSB type_args = separated_list(COMMA, type_exp) RSB { TCon (pattern_name, type_args) }
  | LPAREN ty = type_exp RPAREN { ty }
  | tv = TYPE_VAR { TVar tv }
  | FORALL tv = TYPE_VAR DOT ty = type_exp { TForall (tv, KTy, ty) }
  | LCB v = VAR COLON inner = refine_base_ty VBAR p = pred_expr RCB
    { TRefine (v, inner, p) }

(* Inner type of a refinement is restricted to the base scalar types: refinements
   on ref/cont/function/ADT types are out of scope for this iteration. *)
refine_base_ty:
  | TINT { TInt }
  | TBOOL { TBool }

(* Predicate sub-grammar. A strict subset of [expr] that the SMT encoder can
   handle: integer/bool literals, variables, linear arithmetic (validated at
   typecheck time to avoid non-linear products), comparisons, boolean
   connectives, parentheses. Returns an [SLsyntax.expr]. *)
pred_expr:
  | pred_simple { $1 }
  | e1 = pred_expr ADD e2 = pred_expr { Arith(e1, AAdd, e2) }
  | e1 = pred_expr SUB e2 = pred_expr { Arith(e1, ASub, e2) }
  | e1 = pred_expr MULT e2 = pred_expr { Arith(e1, AMult, e2) }
  | e1 = pred_expr DIV e2 = pred_expr { Arith(e1, ADiv, e2) }
  | e1 = pred_expr PERC e2 = pred_expr { Arith(e1, AMod, e2) }
  | e1 = pred_expr CMPEQ e2 = pred_expr { Cmp(e1, CEq, e2) }
  | e1 = pred_expr NEQ e2 = pred_expr { Cmp(e1, CNeq, e2) }
  | e1 = pred_expr LTS e2 = pred_expr { Cmp(e1, CLt, e2) }
  | e1 = pred_expr GTS e2 = pred_expr { Cmp(e1, CGt, e2) }
  | e1 = pred_expr LEQ e2 = pred_expr { Cmp(e1, CLe, e2) }
  | e1 = pred_expr GEQ e2 = pred_expr { Cmp(e1, CGe, e2) }
  | e1 = pred_expr CONJ e2 = pred_expr { BArith(e1, BConj, e2) }
  | e1 = pred_expr DISJ e2 = pred_expr { BArith(e1, BDisj, e2) }
  | NEG e = pred_expr { Neg(e) }

pred_simple:
  | VAR { Var $1 }
  | INT { Int $1 }
  | SUB INT { Int (Int.neg $2) }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN e = pred_expr RPAREN { e }

cty_exp:
  // Prefer parsing `... -> t / e` as `... -> (t / e)`.
  // Use parentheses when you mean `(... -> t) / e`.
  | t = type_exp %prec CTY_PURE { CCty (t, EPure) }
  | t = type_exp DIV e = eff_exp { CCty (t, e) }

eff_exp:
  | c1 = cty_exp FATARROW c2 = cty_exp { EAns (c1, c2) }

parameter:
  | name = VAR COLON param_type = type_exp { (name, param_type) }

dist_atom:
  | INT { if $1 = 0 then DZero else if $1 = 1 then DOne else failwith "distance: only 0 and 1 are valid integer literals" }
  | v = TYPE_VAR { DVar v }
  | LPAREN d = dist_exp RPAREN { d }

dist_exp:
  | dist_atom { $1 }
  | d1 = dist_exp ADD d2 = dist_atom { DPlus (d1, d2) }

kind_anno:
  | ATC LPAREN d = dist_exp RPAREN { KATC d }

(* Type parameter with optional kind annotation (defaults to KTy). *)
type_param_decl:
  | tv = TYPE_VAR { (tv, KTy) }
  | tv = TYPE_VAR COLON k = kind_anno { (tv, k) }

top_level:
  | DEF name = VAR
      opt_params = opt_params
      LPAREN params = separated_list(COMMA, parameter) RPAREN
      COLON return_cty = cty_exp
      LCB e = expr RCB {
        let (cap_params, label_params) = opt_params in
        TLAbs (name, cap_params, label_params, params, return_cty, e)
      }
  | DEF name = VAR
      COLON COLON LSB type_params = separated_list(COMMA, type_param_decl) RSB
      opt_params = opt_params
      LPAREN params = separated_list(COMMA, parameter) RPAREN
      COLON return_cty = cty_exp
      LCB e = expr RCB {
        let (cap_params, label_params) = opt_params in
        TLPolyAbs (name, type_params, cap_params, label_params, params, return_cty, e)
      }
  | EFFECT name = CAPITALIZED_VAR LCB l = list(effect_sig) RCB { TLEffSig (name, l) }
  | EXCEPTION name = CAPITALIZED_VAR LCB ops = list(effect_sig) RCB { TLEffZSig (name, ops) }
  | TYPE l = separated_nonempty_list(AND, type_def) { TLType l }
  | OPEN filename = STRING { TLOpen filename }
  | OPEN_C_HEADER filename = STRING { TLOpenC filename }
      
effect_sig:
  | v = VAR COLON LPAREN inputs_ty = separated_list(COMMA, type_exp) RPAREN RARROW return_ty = type_exp 
    { (v, (inputs_ty, return_ty)) }

hdl_anno:
  | DEF { HDef }
  | EXC { HExc }
  | HDL1 { HHdl1 }
  | HDLS { HHdls }

return_clause:
  | RETURN return_var = VAR COLON return_var_ty = type_exp LCB return_body = expr RCB
    { ({return_var; return_var_ty; return_body}: SLsyntax.return_clause) }

hdl_def:
  | op_anno = hdl_anno op_name = VAR LPAREN op_params = separated_list(COMMA, VAR) RPAREN LCB op_body = expr RCB
    { {op_anno; op_name; op_params; op_body} }

heap_value:
  | LCB l = separated_list(COMMA, expr) RCB { l }

recfun:
  | name = VAR LTS captured_set = capability GTS
    opt_params = opt_params
    LPAREN params = separated_list(COMMA, parameter) RPAREN
    COLON return_cty = cty_exp
    LCB e = expr RCB
    { let (cap_params, label_params) = opt_params in
      ({name; captured_set; cap_params; label_params; params; body = e; return_cty} : SLsyntax.fundef) }

match_clause:
  | con_name = CAPITALIZED_VAR RARROW LCB e = expr RCB { ( PTypecon (con_name, []), e) }
  | con_name = CAPITALIZED_VAR LPAREN con_args = separated_nonempty_list(COMMA, VAR) RPAREN RARROW LCB e = expr RCB 
    { (PTypecon(con_name, con_args), e) }

evidence_atom:
  | INT { if $1 = 0 then EZero else failwith "evidence: only 0 is a valid integer literal" }
  | MULT { ENull }
  | v = VAR { EVar v }
  | LPAREN e = evidence_exp RPAREN { e }

evidence_exp:
  | evidence_atom { $1 }
  | e1 = evidence_exp ADD e2 = evidence_atom { EPlus (e1, e2) }

(* Answer-type conversion (ATC) surface syntax:
     []                identity ATC                          (ATCHole)
     T / C => CC       impure extension                      (ATCAns)
     X[CC]             ATC-variable X applied to an ATC      (ATCFill)
     'X                bare ATC-kinded type variable         (ATCVar)
   Nested C is a type_exp (pure cty). To pass an impure C, wrap in parens. *)
atc_cty:
  | t = type_exp { CCty (t, EPure) }
  | LPAREN c = cty_exp RPAREN { c }

atc_exp:
  | LSB RSB { ATCHole }
  | LSB cc = atc_exp RSB { cc }
  | tv = TYPE_VAR LSB cc = atc_exp RSB { ATCFill (tv, cc) }
  | tv = TYPE_VAR { ATCVar tv }
  | t = type_exp DIV c = atc_cty FATARROW cc = atc_exp { ATCAns (t, c, cc) }

app_expr:
  | simple_expr { $1 }
  | e1 = app_expr 
    opt_args = opt_args
    LPAREN args = separated_list(COMMA, expr) RPAREN 
    { let (cap_insts, label_args) = opt_args in
      App { func = e1; cap_insts; label_args; args } }
  | v = app_expr LSB v2 = expr RSB { Get (v, v2) }
  | e = app_expr COLON COLON LSB t_args = separated_list(COMMA, type_exp) RSB 
  { List.fold_left (fun e t_arg -> TypeApp (t_arg, e)) e t_args }

expr:
  | app_expr { $1 }
  | e1 = expr ADD e2 = expr { Arith(e1, AAdd, e2) }
	| e1 = expr SUB e2 = expr { Arith(e1, ASub, e2) }
	| e1 = expr MULT e2 = expr { Arith(e1, AMult, e2) }
	| e1 = expr DIV e2 = expr { Arith(e1, ADiv, e2) }
	| e1 = expr PERC e2 = expr { Arith(e1, AMod, e2) }
	
	| e1 = expr CMPEQ e2 = expr { Cmp(e1, CEq, e2) }
	| e1 = expr NEQ e2 = expr { Cmp(e1, CNeq, e2) }
	| e1 = expr GTS e2 = expr { Cmp(e1, CGt, e2) }
	| e1 = expr LTS e2 = expr { Cmp(e1, CLt, e2) }
	| e1 = expr LEQ e2 = expr { Cmp(e1, CLe, e2) }
	| e1 = expr GEQ e2 = expr { Cmp(e1, CGe, e2) }
	
  | NEG e = expr { Neg(e) }
  | e1 = expr CONJ e2 = expr { BArith (e1, BConj, e2) }
  | e1 = expr DISJ e2 = expr { BArith (e1, BDisj, e2) }
  | NEWREF heap_value { New $2 }
  | v1 = app_expr LSB v2 = expr RSB COLONEQ v3 = expr { Set (v1, v2, v3) }
  | VALDEF x = VAR EQ t1 = expr SEMICOLON t2 = expr %prec HIGHER_THAN_STMT { Let (x, t1, t2) }
  | IF v = expr THEN t1 = expr ELSE t2 = expr { If (v, t1, t2) }
  | RAISE raise_label = VAR DOT raise_op = VAR LSB raise_evidence = evidence_exp SEMICOLON raise_atc = atc_exp RSB LPAREN raise_args = separated_list(COMMA, expr) RPAREN
    { Raise {raise_label; raise_op; raise_evidence; raise_atc; raise_args} }
  | RESUME k = simple_expr v = app_expr { Resume (k, v) }
  | RESUMEFINAL k = simple_expr v = app_expr { ResumeFinal (k, v) }
  | HANDLE
    LTS captured_set = capability GTS
    LCB LSB region_binder = VAR COMMA evidence_binder = VAR RSB FATARROW handle_body = expr RCB
    WITH handler_label = VAR COLON sig_name = CAPITALIZED_VAR
    LCB return_clause = option(return_clause) handler_defs = list(hdl_def) RCB
    { Handle {captured_set; region_binder; evidence_binder; handle_body; handler_label; sig_name; return_clause; handler_defs} }
  | FUN LTS captured_set = capability GTS
      opt_params = opt_params
      LPAREN params = separated_list(COMMA, parameter) RPAREN
      COLON return_cty = cty_exp
      LCB e = expr RCB {
        let (cap_params, label_params) = opt_params in
        Fun {captured_set; cap_params; label_params; params; body = e; return_cty}
      }
  | REC DEF fs = separated_list(AND, recfun) SEMICOLON e = expr { Recdef (fs, e) }
  | e1 = expr SEMICOLON e2 = expr %prec STMT { Stmt (e1, e2) }
  | MATCH e = expr WITH VBAR l = separated_nonempty_list(VBAR, match_clause) { Match {match_expr = e; pattern_matching = l} }
  | con_name = CAPITALIZED_VAR LPAREN args = separated_list(COMMA, expr) RPAREN { Typecon (con_name, [], args) }
  | con_name = CAPITALIZED_VAR { Typecon (con_name, [], []) }
  | con_name = CAPITALIZED_VAR 
    COLON COLON LSB type_args = separated_list(COMMA, type_exp) RSB
    LPAREN args = separated_list(COMMA, expr) RPAREN { Typecon (con_name, type_args, args) }
  | con_name = CAPITALIZED_VAR COLON COLON LSB type_args = separated_list(COMMA, type_exp) RSB { Typecon (con_name, type_args, []) }
  | LPAREN RPAREN { Unit }

simple_expr:
  | VAR { Var $1 }
  | INT { Int $1 }
  | SUB INT { Int (Int.neg $2) } 
  | FLOAT { Float $1 }
  | SUB FLOAT { Float (Float.neg $2) }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | s = STRING { Str s } 
  | c = CHAR { Char c }
  | PRIM { Prim $1 }
  | LPAREN e = expr RPAREN { e }
  
