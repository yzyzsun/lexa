import os
import psutil
import pwd

def get_smt_groups():
    import subprocess
    from collections import defaultdict
    result = subprocess.run(["lscpu", "-e=CPU,Core,Socket"], capture_output=True, text=True)
    lines = result.stdout.strip().splitlines()[1:]  # skip header

    core_map = defaultdict(list)
    for line in lines:
        cpu_id, core_id, socket_id = map(int, line.strip().split())
        key = (socket_id, core_id)
        core_map[key].append(cpu_id)

    # pick one core from each SMT group
    return [min(cpus) for cpus in core_map.values()]

# Get NUM_CPUS from the environment or set a default
try:
    NUM_CPUS = int(os.environ.get("NUM_CPUS", 4))
except ValueError:
    print("Invalid NUM_CPUS value, defaulting to 4")
    NUM_CPUS = 4
bench_CPUs = get_smt_groups()[:NUM_CPUS]  # limit to the first few cores

benchmarks = ["countdown", "fibonacci_recursive", "product_early", "iterator", "nqueens", "generator", "tree_explore", "triples", "resume_nontail", "parsing_dollars", "handler_sieve", "resume_nontail_2", "scheduler", "interruptible_iterator"]
platforms = ["lexa", "lexaz", "effekt", "koka_named", "koka", "ocaml"]

higher_order_benchmarks = ["logger", "listiter"]
benchmarks += higher_order_benchmarks

zero_cost_benchmarks = ["catalan", "bezout", "golomb", "hofstadterq", "karatsuba", "ackermann", "palindrome_partition", "latticepath", "two_threads_ackermann"]
benchmarks += zero_cost_benchmarks

config = {}

username = pwd.getpwuid(os.getuid()).pw_name

for benchmark in benchmarks:
    LEXA_BUILD_COMMAND = f"flock /tmp/dune_lockfile_{username} -c 'lexa main.lx -o main'"
    LEXA_RUN_COMMAND = "./main {IN}"
    config[("lexa", benchmark)] = {
        "build": LEXA_BUILD_COMMAND, "run": LEXA_RUN_COMMAND,
    }

    LEXAZ_BUILD_COMMAND = f"flock /tmp/dune_lockfile_{username} -c 'lexa main.lx -o main'"
    LEXAZ_RUN_COMMAND = "./main {IN}"
    config[("lexaz", benchmark)] = {
        "build": LEXAZ_BUILD_COMMAND, "run": LEXAZ_RUN_COMMAND,
    }

    OCAML_BUILD_COMMAND = "flock /tmp/opam_lockfile -c 'opam exec --switch=5.3.0 -- ocamlopt -O3 -o main -I $(opam var lib)/multicont multicont.cmxa main.ml -o main'"
    OCAML_RUN_COMMAND = "./main {IN}"
    config[("ocaml", benchmark)] = {
        "build": OCAML_BUILD_COMMAND, "run": OCAML_RUN_COMMAND,
    }

    KOKA_BUILD_COMMAND = "koka -O3 -v0 -o main main.kk  && chmod +x main"
    KOKA_RUN_COMMAND = "./main {IN}"
    config[("koka", benchmark)] = {
        "build": KOKA_BUILD_COMMAND, "run": KOKA_RUN_COMMAND,
    }

    KOKA_NAMED_BUILD_COMMAND = "koka -O3 -v0 -o main main.kk  && chmod +x main"
    KOKA_NAMED_RUN_COMMAND = "./main {IN}"
    config[("koka_named", benchmark)] = {
        "build": KOKA_NAMED_BUILD_COMMAND, "run": KOKA_NAMED_RUN_COMMAND,
    }

    EFFEKT_BUILD_COMMAND = "effekt_latest.sh --backend ml --compile main.effekt  && mlton -default-type int64 -output main out/main.sml"
    EFFEKT_RUN_COMMAND = "./main {IN}"
    config[("effekt", benchmark)] = {
        "build": EFFEKT_BUILD_COMMAND, "run": EFFEKT_RUN_COMMAND,
    }

# Adjustments
config[("effekt", "handler_sieve")]["build"] = "effekt_latest.sh --backend chez-lift --compile main.effekt"
config[("effekt", "handler_sieve")]["run"] = "scheme --script out/main.ss {IN} 0"
config[("effekt", "handler_sieve")]["adjust_warmup"] = True
config[("effekt", "generator")]["build"] = "effekt_latest.sh --backend chez-lift --compile main.effekt"
config[("effekt", "generator")]["run"] = "scheme --script out/main.ss {IN} 0"
config[("effekt", "generator")]["adjust_warmup"] = True
config[("effekt", "scheduler")]["build"] = "effekt_latest.sh --backend js --compile main.effekt"
config[("effekt", "scheduler")]["run"] = "node --eval \"require(\'\"\'./out/main.js\'\"\').main()\" -- _ {IN} 0"
config[("effekt", "scheduler")]["adjust_warmup"] = True
config[("effekt", "interruptible_iterator")]["build"] = "effekt_latest.sh --backend js --compile main.effekt"
config[("effekt", "interruptible_iterator")]["run"] = "node --eval \"require(\'\"\'./out/main.js\'\"\').main()\" -- _ {IN} 0"
config[("effekt", "interruptible_iterator")]["adjust_warmup"] = True
config[("effekt", "two_threads_ackermann")]["build"] = "effekt_latest.sh --backend chez-lift --compile main.effekt"
config[("effekt", "two_threads_ackermann")]["run"] = "scheme --script out/main.ss {IN} 0"

# Known Failures
config[("koka", "interruptible_iterator")]["fail_reason"] = "Koka type system limitation"
config[("koka_named", "scheduler")]["fail_reason"] = "Koka internal compiler error"
config[("koka_named", "two_threads_ackermann")]["fail_reason"] = "Koka internal compiler error"
config[("effekt", "bezout")]["fail_reason"] = "Stack Overflow"
config[("lexaz", "scheduler")]["fail_reason"] = "Not implemented"
config[("lexaz", "interruptible_iterator")]["fail_reason"] = "Not implemented"
config[("lexaz", "resume_nontail_2")]["fail_reason"] = "Not implemented"
for benchmark in zero_cost_benchmarks + higher_order_benchmarks:
    for platform in ["koka", "ocaml"]:
        config[(platform, benchmark)]["fail_reason"] = "Not implemented"


config[("effekt", "scheduler")]["scale"] = 1000
config[("effekt", "interruptible_iterator")]["scale"] = 1000
config[("ocaml", "interruptible_iterator")]["scale"] = 100
config[("koka", "resume_nontail_2")]["scale"] = 100
config[("koka_named", "resume_nontail_2")]["scale"] = 100

for platform in platforms:
    config[(platform, "countdown")]["bench_input"] = 200000000
    config[(platform, "fibonacci_recursive")]["bench_input"] = 42
    config[(platform, "product_early")]["bench_input"] = 100000
    config[(platform, "iterator")]["bench_input"] = 40000000
    config[(platform, "nqueens")]["bench_input"] = 12
    config[(platform, "generator")]["bench_input"] = 25
    config[(platform, "tree_explore")]["bench_input"] = 16
    config[(platform, "triples")]["bench_input"] = 300
    config[(platform, "resume_nontail")]["bench_input"] = 10000
    config[(platform, "parsing_dollars")]["bench_input"] = 20000
    config[(platform, "handler_sieve")]["bench_input"] = 10000
    config[(platform, "scheduler")]["bench_input"] = 3000
    config[(platform, "interruptible_iterator")]["bench_input"] = 3000
    # config[(platform, "concurrent_search")]["bench_input"] = 13
    config[(platform, "resume_nontail_2")]["bench_input"] = 10000

    config[(platform, "logger")]["bench_input"] = 50000000
    config[(platform, "listiter")]["bench_input"] = 100000

    config[(platform, "catalan")]["bench_input"] = 18
    config[(platform, "bezout")]["bench_input"] = 1000000
    config[(platform, "golomb")]["bench_input"] = 60
    config[(platform, "hofstadterq")]["bench_input"] = 38
    config[(platform, "karatsuba")]["bench_input"] = 32767
    config[(platform, "ackermann")]["bench_input"] = 4
    config[(platform, "palindrome_partition")]["bench_input"] = None
    config[(platform, "latticepath")]["bench_input"] = 16
    config[(platform, "two_threads_ackermann")]["bench_input"] = 1000000