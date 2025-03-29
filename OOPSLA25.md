This guide will help you reproduce the results in the OOPSLA 2025 submission.

## Getting started

### Prerequisites
* Supported platform: x86-64.
* 32GB of RAM recommended for building.

### Setup using Nix
1. **Install Nix**: Follow the [instructions](https://nixos.org/download.html) to install Nix on your system.
2. **Clone the repository**: Clone the Lexa repo to your local machine.
3. **Build the development environment**: Run `nix develop` in the repository root. This could take up to an hour, or a few seconds if you allow Nix to use the pre-built cache.
4. **Build the project**: Run `dune build` to build the compiler.

## Reproducing the results in the OOPSLA 2025 Submission
1. Follow the instructions above to set up the project.
2. **Table 1**: Run `cd scripts; python bench.py --systems lexa lexaz --all-zero-cost-benchmarks`. The result will be saved in `./runtimes.csv`.
3. **Two threads figure**: Run `cd scripts/OOPSLA25; python plots.py --plot-only ./plotting_runtimes_3_25.csv`. The plot will be saved in `./two-threads.pdf`. To plot using fresh data, run `python ./plots.py`.
