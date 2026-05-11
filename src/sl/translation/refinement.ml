(** SMT-LIB encoder + Z3 subprocess discharge for refinement-type VCs.

    The encoder targets QF_LIA (quantifier-free linear integer arithmetic
    with booleans). Multiplication is restricted at parse-time to "literal
    times anything", so the encoded goal stays in QF_LIA.

    A discharge call shells out to [z3 -in -smt2], writes the script to
    stdin, and reads "sat" / "unsat" / "unknown" off stdout. Setting
    [LEXA_DUMP_SMT=1] makes [discharge] write each script to /tmp for
    inspection. *)

open SLsyntax
open Common

type smt_result = Sat | Unsat | Unknown

(** SMT-LIB sort for a Lexa value type, if encodable. We only encode the two
    scalar theories we have axioms for. Refinement-wrapped base types collapse
    to the underlying base. Anything else is None and the caller should treat
    it as an opaque (uninterpreted) symbol — sound but coarse. *)
let rec smt_sort_of_ty (ty: ty) : string option =
  match ty with
  | TInt -> Some "Int"
  | TBool -> Some "Bool"
  | TRefine (_, inner, _) -> smt_sort_of_ty inner
  | _ -> None

(** Translate refinement logic terms and formulas into SMT-LIB. Unsupported
    logical nodes raise; callers that may see residual predicate variables
    should use [pred_to_smt_opt]. *)
let rec pred_term_to_smt (t: pred_term) : string =
  match t with
  | PTInt n ->
    if n >= 0 then string_of_int n
    else Printf.sprintf "(- %d)" (-n)
  | PTBool true -> "true"
  | PTBool false -> "false"
  | PTVar x -> x
  | PTArith (t1, op, t2) ->
    let s = match op with
      | AAdd -> "+" | ASub -> "-" | AMult -> "*"
      | ADiv -> "div" | AMod -> "mod"
    in
    Printf.sprintf "(%s %s %s)" s (pred_term_to_smt t1) (pred_term_to_smt t2)
  | PTUnit ->
    failwith
      (Printf.sprintf "pred_term_to_smt: unsupported unit term: %s" (pred_term_to_str t))

and pred_to_smt (p: pred) : string =
  match p with
  | PAtom t -> pred_term_to_smt t
  | PCmp (t1, op, t2) ->
    let s = match op with
      | CEq -> "=" | CNeq -> "distinct"
      | CLt -> "<" | CGt -> ">"
      | CLe -> "<=" | CGe -> ">="
    in
    Printf.sprintf "(%s %s %s)" s (pred_term_to_smt t1) (pred_term_to_smt t2)
  | PBArith (p1, op, p2) ->
    let s = match op with BConj -> "and" | BDisj -> "or" in
    Printf.sprintf "(%s %s %s)" s (pred_to_smt p1) (pred_to_smt p2)
  | PApp _ ->
    failwith
      (Printf.sprintf "pred_to_smt: unsubstituted predicate variable: %s" (pred_to_str p))
  | PNeg p' -> Printf.sprintf "(not %s)" (pred_to_smt p')

(** Try to encode a logic term/formula as an SMT-LIB term. Returns None on
    residual predicate variables or unit terms. *)
let rec pred_term_to_smt_opt (t: pred_term) : string option =
  match t with
  | PTInt _ | PTBool _ | PTVar _ -> Some (pred_term_to_smt t)
  | PTArith (t1, op, t2) ->
    (match pred_term_to_smt_opt t1, pred_term_to_smt_opt t2 with
     | Some s1, Some s2 ->
       let s = match op with
         | AAdd -> "+" | ASub -> "-" | AMult -> "*"
         | ADiv -> "div" | AMod -> "mod"
       in Some (Printf.sprintf "(%s %s %s)" s s1 s2)
     | _ -> None)
  | PTUnit -> None

and pred_to_smt_opt (p: pred) : string option =
  match p with
  | PAtom t -> pred_term_to_smt_opt t
  | PCmp (t1, op, t2) ->
    (match pred_term_to_smt_opt t1, pred_term_to_smt_opt t2 with
     | Some s1, Some s2 ->
       let s = match op with
         | CEq -> "=" | CNeq -> "distinct"
         | CLt -> "<" | CGt -> ">"
         | CLe -> "<=" | CGe -> ">="
       in Some (Printf.sprintf "(%s %s %s)" s s1 s2)
     | _ -> None)
  | PBArith (p1, op, p2) ->
    (match pred_to_smt_opt p1, pred_to_smt_opt p2 with
     | Some s1, Some s2 ->
       let s = match op with BConj -> "and" | BDisj -> "or" in
        Some (Printf.sprintf "(%s %s %s)" s s1 s2)
     | _ -> None)
  | PApp _ -> None
  | PNeg p' ->
    (match pred_to_smt_opt p' with
     | Some s -> Some (Printf.sprintf "(not %s)" s)
     | None -> None)

(** Refinement hypotheses extracted from the typing environment: for every
    [(x, {v: T | P})] in scope where T is encodable, we'll emit P[v := x] as
    an asserted formula. *)
let hyps_of_term_vars (term_vars: (string * ty) list) : pred list =
  List.filter_map (fun (x, ty) ->
    match ty with
    | TRefine (v, inner, p) when smt_sort_of_ty inner <> None ->
      Some (subst_var_in_pred p v (PTVar x))
    | _ -> None
  ) term_vars

(** Variable declarations we need for the script. We declare every term
    variable that has an encodable sort (after stripping refinement). *)
let decls_of_term_vars (term_vars: (string * ty) list) : (string * string) list =
  List.filter_map (fun (x, ty) ->
    match smt_sort_of_ty ty with
    | Some s -> Some (x, s)
    | None -> None
  ) term_vars

(** Build a complete SMT-LIB script that asks whether [hyps |= goal]. We do
    that by asserting the conjunction of [hyps] and [(not goal)] and asking
    [check-sat]: [unsat] means valid (entailment holds); [sat] means a
    counterexample exists. *)
let build_script
    (decls: (string * string) list)
    (hyps: pred list)
    (goal: pred)
  : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(set-logic ALL)\n";
  Buffer.add_string buf "(set-option :produce-models true)\n";
  List.iter (fun (x, sort) ->
    Buffer.add_string buf (Printf.sprintf "(declare-const %s %s)\n" x sort)
  ) decls;
  List.iter (fun h ->
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" (pred_to_smt h))
  ) hyps;
  Buffer.add_string buf
    (Printf.sprintf "(assert (not %s))\n" (pred_to_smt goal));
  Buffer.add_string buf "(check-sat)\n";
  Buffer.contents buf

(** Write the SMT-LIB script to a temp file, run [z3 -smt2 <file>], and parse
    the first non-empty line. We avoid piping to [z3 -in] because OCaml's
    buffered Unix pipes have produced deadlocks with that mode in practice
    and the temp-file flow is simpler to reason about. The [LEXA_DUMP_SMT]
    env var keeps the temp file in place so it can be re-run by hand. *)
let dump_counter = ref 0

let discharge (script: string) : smt_result =
  incr dump_counter;
  let keep =
    match Sys.getenv_opt "LEXA_DUMP_SMT" with Some _ -> true | None -> false
  in
  let path =
    if keep then Printf.sprintf "/tmp/lexa_smt_%d.smt2" !dump_counter
    else Filename.temp_file "lexa_smt_" ".smt2"
  in
  let oc = open_out path in
  output_string oc script;
  close_out oc;
  if keep then Printf.eprintf "[lexa-smt] wrote %s\n" path;
  (* Read Z3's first line of output. We use [Unix.open_process_in] so the
     parent only reads stdout (Z3 writes nothing on stderr for the inputs
     we generate); this avoids the pipe-deadlock problem we hit with
     [create_process] + manual pipe wiring. *)
  let cmd = Printf.sprintf "z3 -smt2 %s" (Filename.quote path) in
  let ic = Unix.open_process_in cmd in
  let line = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  if not keep then (try Sys.remove path with _ -> ());
  match String.trim line with
  | "unsat" -> Unsat
  | "sat" -> Sat
  | _ -> Unknown

(** [entails ~term_vars hyps goal] asks whether the typing context
    [term_vars]'s refinement predicates plus extra [hyps] entail [goal].
    Returns [true] iff Z3 says [unsat] on [(hyps /\ \neg goal)]. *)
let entails ~term_vars (extra_hyps: pred list) (goal: pred) : bool =
  let env_hyps = hyps_of_term_vars term_vars in
  let all_hyps = env_hyps @ extra_hyps in
  let decls = decls_of_term_vars term_vars in
  let script = build_script decls all_hyps goal in
  match discharge script with
  | Unsat -> true
  | Sat | Unknown -> false
