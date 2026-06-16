# `arm` benchmarks migrated to Lexa

The OCaml-with-effects programs in the top-level `arm/` directory (each a
`*-SAT.ml` / `*-UNSAT.ml` pair carrying a refinement `[@@@assert ...]`) migrated
to Lexa's refinement-checked surface syntax.

Verify everything with:

```sh
bash examples/arm/check.sh      # *_unsat.lx must FAIL typecheck, all others PASS
```

All migrated ARM files live in this directory.  SAT examples omit a suffix
(`queue_1.lx`); negative examples use `_unsat.lx`.

## Compiler features added for these migrations

To migrate faithfully (rather than weaken the assertions), the typechecker was
extended — see `src/sl/translation/{refinement,common,typecheck}.ml`,
`src/sl/syntax/SLsyntax.ml`, `src/sl/parser/parser.mly`:

1. **Refinement types over algebraic data types ("refinements inside lists").**
   The predicate language gained a constructor-application term
   (`PTCon`, e.g. `z == Some(n / m)`, `z == Cons(1, Cons(2, Nil))`). Non-
   parametric datatypes are now emitted as SMT-LIB `declare-datatypes`, so the
   solver reasons with **constructor distinctness, injectivity and selectors**
   for free. This is what makes `Some`/`None`, `Failure`/`Success v`, and exact
   list/tuple values refinable.
2. **Matching a refined scrutinee.** `match` strips an outer refinement, and
   each branch now learns `scrutinee == C(y..)`, so the scrutinee's refinement
   propagates onto the bound payload variables (matching `Success y` on a value
   refined to `Success 5` yields `y == 5`). This is what lets the backtracking
   `amb`/`select`/`bfs` handlers keep a precise answer type.
3. **Conditionals against an impure expected type.** `check_cty` now pushes the
   full expected computation type (value refinement *and* answer) into both
   branches of an `if`, under the branch path conditions — previously only the
   pure case did this. Refined data values therefore flow leaf-by-leaf through a
   handler's answer type (e.g. `Some(n/m)` in the `safe_div` handler body).
4. **Checking-mode for `match` / `let` / `stmt`.** `check_cty` and
   `type_expr_with_final` now thread the expected answer (and value refinement)
   through `match`, `let` and `;`, so an effect operation performed inside a
   `match` branch recovers its ATC, and a refined result flows through the
   control flow. This is what makes **effect-performing recursive functions**
   typecheck — e.g. the real recursive `select_from` list scan of `amb` and the
   recursive `counter` in `state_easy`. A cross-region `raise` inside such a
   function carries an explicit identity ATC `[[]]`; the region path from the
   function to the handler is otherwise unresolved at that point.

## Per-example faithfulness audit

**A. Faithful — full program structure preserved, original-style assertion.**
- `amb_1/2/3` — the **real recursive `select_from` list scan** (performing
  `select` per element under the backtracking handler).  `amb_1` proves the
  empty list ⇒ `Failure` *from the scan*; `amb_2/3` prove the search returns
  `Failure` or *exactly* `Success(target)`.
- `amb_3_simpl` — the simplified boolean `amb` variant is exact: the
  multi-shot handler computes boolean disjunction over the explored branches.
- `choose_sum`, `choose_max`, `choose_all`, `distribution`, `expectation`,
  `shift` — exact answer-modifying `hdl_s` translations.  The operations carry
  the continuation-answer refinements needed by the non-endomorphic combiners
  (sum, max, list accumulation, weighted average, and nested `k(k(k n))`).
- `deferred_1/2` — preserve the `Value | Thunk(ctx, thunk)` cache state:
  `Force` either returns the cached value or evaluates the stored thunk once
  and resumes with `Value(v)`.
- `io_read_1/2/3` — the **real recursive `go n` read loop**.  `io_read_1/2`
  prove the loop returns `Ok`; `io_read_3` keeps the loop with an aborting
  (exceptional) read, proving `Err` when reading the empty list.
- `safe_div_1/2` — the conditional, the raise, and the division are preserved:
  `m=0 ⇒ None`; `m≠0 ⇒ None or Some(n/m)`.
- `select` — preserves the real choice and test structure.
- `queue_1/2` — preserves the original `add 42; get; get` stateful queue
  protocol with explicit before/after queue states.
- `state` — preserves the recursive countdown and proves the exact
  `result == init` invariant.
- `state_easy` — preserves the recursive countdown and proves the original
  nonnegative result invariant.
- `transaction` — preserves the transaction update/lookup/abort protocol over
  explicit handler state.  The final committed/reflected ref value is modelled
  as the handler answer (`0` on abort, `42` on commit).
- `io_write_1/2` — preserve the recursive `go li` write scan; the accumulated
  output list is represented by its count (`0` for empty input, `>=1` for
  non-empty input).

**B. Faithful observable benchmark, different representation.**
- `bfs`/`bfs_simpl` preserve the observable nondeterministic search result and
  exact result datatype invariant, but use Lexa's multi-shot backtracking shape
  rather than the OCaml ref queue of suspended continuations.
- `modulus` preserves the concrete benchmark functional `f a = 0 * a 10`: it
  calls the sequence at `10`, updates the handler state to the queried modulus,
  and compares the two exact modulus results.  It does not expose the fully
  general higher-order `mu f sequence` combinator as a reusable function.
- `round_robin` keeps the concrete benchmark's four scheduled increments and
  proves the original `result >= init` assertion with exact answer
  modification, but not the mutable continuation queue scheduler itself.

**C. Still simplified by continuation-storage limits.**
- `yield` — the original `iterator` is `Result tree | Susp of int * (int ->
  iterator)`, where the function is a captured continuation.  The current file
  preserves the single-leaf yield/resume behaviour, but not the full
  continuation-storing iterator over arbitrary trees.

## Handler templates
- `hdl_s` multi-shot (backtracking / nondeterminism): `amb`, `bfs`, `select`,
  `choose_*`, `expectation`, `shift`.
- `hdl_1` value/state: `deferred`, `io_read`, `yield`, `modulus`, `queue`.
- `hdl_1` forward-threaded state (the `tick`/`state` idiom): `io_write`,
  `state`, `state_easy`.
- `exc` exceptional effect (abort): `safe_div`, `io_read_3`.

## Remaining constraints
- Effect-performing functions (incl. recursion) now typecheck inside a handler
  body (see feature 4), but a cross-region `raise` needs an explicit ATC
  annotation (`[[]]` for an endomorphic handler); the region distance from the
  function to the handler is not yet inferred automatically.
- Parametric datatypes (e.g. `list::['a]`) are not yet SMT-encoded; the migrated
  ADTs are monomorphic.
- Captured continuations can be resumed by handlers, but the ARM programs that
  store continuations inside long-lived scheduler/iterator data structures
  still need more surface/typechecker work to translate directly.
