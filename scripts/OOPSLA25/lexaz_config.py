import os
import psutil
import pwd

# On my machine with i5-13600K, the 6 performance cores
# uses hyperthreading, so we have 6 physical cores as follow
with open('/proc/cpuinfo') as f:
    for line in f:
        if 'model name' in line:
            cpu_model = line.split(': ')[1].strip()
            break
if "i5-13600K" in cpu_model:
    bench_CPUs = ["0", "2", "4", "6", "8", "10"]
elif "7700X" in cpu_model:
    # bench_CPUs = ["0", "1", "2", "3", "4", "5"]
    bench_CPUs = ["1", "2", "3", "4", "5"]
else:
    # list all the physical cores
    bench_CPUs = list(range(psutil.cpu_count()))
    if len(bench_CPUs) > 4:
        # do not use too much CI resources
        bench_CPUs = bench_CPUs[:4]

benchmarks = ["two_threads_ackermann"]
platforms = ["lexa", "lexaz"]

config = {}

for benchmark in benchmarks:
    LEXA_BUILD_COMMAND = "flock /tmp/dune_lockfile -c 'lexa main.lx -o main'"
    LEXA_RUN_COMMAND = "./main {IN}"
    config[("lexa", benchmark)] = {
        "build": LEXA_BUILD_COMMAND, "run": LEXA_RUN_COMMAND,
    }

    LEXAZ_BUILD_COMMAND = "flock /tmp/dune_lockfile -c 'lexa main.lx -o main --lexaz'"
    LEXAZ_RUN_COMMAND = "./main {IN}"
    config[("lexaz", benchmark)] = {
        "build": LEXAZ_BUILD_COMMAND, "run": LEXAZ_RUN_COMMAND,
    }

for platform in platforms:
    config[(platform, "two_threads_ackermann")]["bench_input"] = 1000000