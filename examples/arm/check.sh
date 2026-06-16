#!/usr/bin/env bash
# Verify migrated arm examples: *_unsat.lx must FAIL typecheck, others must PASS.
cd "$(dirname "$0")/../.."
pass=0; fail=0
for f in examples/arm/*.lx; do
  base=$(basename "$f" .lx)
  out=$(dune exec lexac -- "$f" 2>&1)
  if echo "$out" | grep -q "Typechecking passed!"; then res=PASS; else res=FAIL; fi
  case "$base" in
    *_unsat) want=FAIL ;;
    *) want=PASS ;;
  esac
  if [ "$res" = "$want" ]; then echo "  ok   $base ($res)"; pass=$((pass+1));
  else echo "  WRONG $base (got $res, want $want)"; fail=$((fail+1)); fi
done
echo "--- $pass ok, $fail wrong ---"
