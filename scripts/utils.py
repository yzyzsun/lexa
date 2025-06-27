import subprocess
from concurrent.futures import ThreadPoolExecutor
from threading import current_thread
import re
import os

from config import bench_CPUs

def print_message(message, short=False):
    if short:
        print(f"=> {message} <=")
    else:
        print(f"{'='*len(message)}\n{message}\n{'='*len(message)}")

def run_processe(command, cwd):
    try:
        return subprocess.run(command, check=True, text=True, capture_output=True, shell=True, cwd=cwd)
    except subprocess.CalledProcessError as e:
        print(f"Error {e.returncode}\nCommand: {command}\nAt: {cwd}\n")
        print(e.stderr)
        raise e

def parse_output(output_file):
    pairs = []
    f = open(output_file, 'r')
    for line in f.readlines():
        command = line.split(",")[0]
        n = int(re.search(r'\d+', command).group())
        mean_time = float(line.split(",")[1])
        pairs.append((n, mean_time))
    return pairs

def build(path, build_command):
    thread_id = int(current_thread().getName().split('_')[1])
    CPU = bench_CPUs[thread_id]
    print_message(f"Building {path}")
    taskset_cmd = f"taskset -c {CPU} {build_command}"
    try:
        run_processe(taskset_cmd, path)
        print_message("Done building")
    except Exception as e:
        print_message("Failed building", short=True)
        return False
    return True

def bench(path, run_command, input, adjust_warmup, quick=False, precise=False):
    thread_id = int(current_thread().getName().split('_')[1])
    CPU = bench_CPUs[thread_id]
    print_message(f"Benchmarking {path}")
    if quick:
        hyperfine_cmd = f"hyperfine --shell none --warmup 0 -M 2 --time-unit millisecond '{run_command.format(IN=input)}'"
    else:
        if precise:
            hyperfine_cmd = f"hyperfine --shell none --warmup 5 --min-runs 30 --time-unit millisecond '{run_command.format(IN=input)}'"
        else:
            hyperfine_cmd = f"hyperfine --shell none --warmup 5 --time-unit millisecond '{run_command.format(IN=input)}'"

    taskset_cmd = f"taskset -c {CPU} {hyperfine_cmd} "
    try:
        result = run_processe(taskset_cmd, path)
        matches = re.search(r"Time \(mean ± σ\):\s+(\d+\.\d+)\sms\s+±\s+(\d+\.\d+)\sms", result.stdout)
        mean_mili = int(float(matches.group(1)))
        std_mili = int(float(matches.group(2)))
        print_message(f"Done benchmarking {path}")

        if adjust_warmup:
            warmup_overhead_mili = bench_warnup_overhead(path, run_command, CPU)
            mean_mili -= warmup_overhead_mili
        return (mean_mili, std_mili)
    except Exception as e:
        print_message("Failed benchmarking", short=True)
        return (None, None)

def build_and_bench(path, build_command, run_command, input, adjust_warmup, quick=False):
    if not build(path, build_command):
        return (None, None)
    return bench(path, run_command, input, adjust_warmup, quick)

def bench_warnup_overhead(path, run_command, CPU):
    print_message(f"Estimating warmup for {path}")
    run_command = "time " + run_command.format(IN=0)
    taskset_cmd = f"taskset -c {CPU} {run_command}"
    overheads_mili = []
    for _ in range(100):
        out = run_processe(taskset_cmd, path)
        internal_nano = int(re.search(r"Nanosecond used: (\d+)", out.stdout).group(1))
        external_sec = float(re.search(r"(\d+\.\d+)user", out.stderr).group(1))
        overheads_mili.append(external_sec * 1000 - internal_nano / 1e6)
    overheads_mili = overheads_mili[10:] # Discard first 10 runs
    overhead = int(sum(overheads_mili) / len(overheads_mili))
    print_message(f"Estimated warmup overhead: {overhead} ms")
    return overhead
