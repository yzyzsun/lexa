from config import config, platforms, benchmarks, bench_CPUs
from utils import build_and_bench
import os
from concurrent.futures import ThreadPoolExecutor
import numpy as np
import argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--quick", action="store_true")
    parser.add_argument("--all-systems", action="store_true")
    args = parser.parse_args()

    config_tups = [(platform, benchmark, params) for (platform, benchmark), params in config.items()]
    config_tups.sort(key=lambda x: (platforms.index(x[0]), benchmarks.index(x[1])))

    if not args.all_systems:
        config_tups = [c for c in config_tups if c[0] == "lexa"]

    results = []

    result_txt = "runtimes.txt"
    result_csv = "runtimes.csv"
    if os.path.exists(result_txt):
        os.rename(result_txt, result_txt + ".bak")
    if os.path.exists(result_csv):
        os.rename(result_csv, result_csv + ".bak")

    with ThreadPoolExecutor(max_workers=len(bench_CPUs)) as executor:
        results_generator = executor.map(
            lambda c: 
                (c[0], 
                c[1], 
                build_and_bench(f"../benchmarks/{c[0]}/{c[1]}", c[2]["build"], c[2]["run"], c[2]["bench_input"], adjust_warmup=c[2].get("adjust_warmup", False), quick=args.quick)
                    * c[2].get("scale", 1)
                )
                if "fail_reason" not in c[2] else (c[0], c[1], None),
            config_tups
        )
        with open(result_txt, 'w') as f:
            for platform, benchmark, (mean_mili, std_mili) in results_generator:
                results += [(platform, benchmark, mean_mili, std_mili)]
                f.write(f"{platform:<15} {benchmark:<30} {mean_mili:<10} {std_mili}\n")
                f.flush()

    import pandas as pd
    df = pd.DataFrame(results, columns=["platform", "benchmark", "mean_mili", "std_mili"])
    pivoted_df = df.pivot_table(index="benchmark", columns="platform", values=["mean_mili", "std_mili"], sort=False)
    pivoted_df.to_csv(result_csv)

if __name__ == "__main__":
    main()