import matplotlib
import matplotlib.pyplot as plt
try:
    import scienceplots # In nix, it is installed through a manual installation
except ImportError:
    pass
from mpl_toolkits.axisartist.axislines import AxesZero
import subprocess
from concurrent.futures import ThreadPoolExecutor
import re
import argparse
import pandas as pd
import numpy as np

import sys
sys.path.append("..")
from config import config, bench_CPUs

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42

pd.options.mode.copy_on_write = True

matplotlib.use("pgf")
preamble = r'\usepackage{fontspec}\setmainfont{Linux Libertine O}\setmonofont[Scale=MatchLowercase]{JetBrains Mono}\usepackage{xcolor}'
params = {
    'font.family': 'serif',
    'text.usetex': True,
    # 'text.latex.unicode': True,
    'pgf.rcfonts': False,
    'pgf.texsystem': 'xelatex',
    'pgf.preamble': preamble,
    'font.size': 12,
    'xtick.labelsize': 8,
    'ytick.labelsize': 8,
}
plt.rcParams.update(params)


plt.rc('text.latex', preamble=r'\usepackage{amsmath}')


import sys, os
chemin_actuel = os.path.dirname(os.path.abspath(__file__))
chemin_parent = os.path.dirname(chemin_actuel)
sys.path.append(chemin_parent)

from utils import *

plt.style.use(['science'])#, "no-latex"])

def plot_df(df, dirname):
    fig, ax = plt.subplots(figsize=(6, 3))

    markers = {'lexa': 'o', 'lexaz': '^'}
    colors = {'lexa': 'blue', 'lexaz': 'green'}

    platform_mappper = {'lexa': 'Stock Lexa', 'lexaz': 'Zero-cost Lexa'}

    for platform in ['lexa', 'lexaz']:
        platform_data = df[df['platform'] == platform]
        ax.plot(platform_data['n'], platform_data['time_mili'], label=platform_mappper[platform])

    ax.set_xlabel('Scheduling Time Slice')
    ax.set_ylabel('Time (milliseconds)')
    ax.set_title('Two-threads Running with Stock Lexa and Zero-cost Lexa')
    ax.legend(title='Platform')

    # plt.ticklabel_format(axis='y', style='plain')
    ax.grid(True)

    filename = "two-threads.pdf"
    plt.savefig(dirname + filename, dpi=600)


def main(config):
    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--plot-only", type=str, default=None)
    parser.add_argument("--quick", action="store_true")
    parser.add_argument("--output-dir", type=str, default='./')
    args = parser.parse_args()

    if not args.plot_only:
        result_txt = "plotting_runtimes.txt"
        result_csv = "plotting_runtimes.csv"

        config = {key: value for key, value in config.items() if key[0] in ["lexa", "lexaz"] and key[1] == "two_threads_ackermann" and key[1] == "two_threads_ackermann"}
        # Build
        config_tups = [(platform, benchmark, 0, params) for (platform, benchmark), params in config.items()]
        with ThreadPoolExecutor(max_workers=len(bench_CPUs)) as executor:
            executor.map(
                lambda c: build(f"../../benchmarks/{c[0]}/{c[1]}", c[3]["build"]) if "fail_reason" not in c[3] else None,
                config_tups
            )


        # Run for sequence of inputs
        results = []
        inputs = [
            i for i in range(10000, 80000, 5000)]
        config_tups = [(platform, benchmark, i, params) for (platform, benchmark), params in config.items() for i in inputs]
        with ThreadPoolExecutor(max_workers=len(bench_CPUs)) as executor:
            results_generator = executor.map(
                lambda c: 
                    (c[0], 
                    c[1],
                    c[2],
                    bench(f"../../benchmarks/{c[0]}/{c[1]}", c[3]["run"], c[2], c[3].get("adjust_warmup", False), quick=args.quick)
                        * c[3].get("scale", 1))
                    if "fail_reason" not in c[3] else (c[0], c[1], c[2], None),
                config_tups
            )
            with open(result_txt, 'w') as f:
                for platform, benchmark, i, (mean_mili, std_mili) in results_generator:
                    results += [(platform, benchmark, i, mean_mili, std_mili)]
                    f.write(f"{platform:<15} {benchmark:<30} {i:<10} {mean_mili:<10} {std_mili:<10}\n")
                    f.flush()
        df = pd.DataFrame(results, columns=["platform", "benchmark", "n", "time_mili", "std_mili"])
        df.to_csv(result_csv)
    else:
        df = pd.read_csv(args.plot_only)


    # Plot
    plot_df(df, args.output_dir)

if __name__ == "__main__":
    main(config)
