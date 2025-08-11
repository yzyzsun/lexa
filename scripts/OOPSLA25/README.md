# Introduction

This artifact accompanies the OOPSLA 2025 submission "Zero-Cost Lexical Effect Handlers". It is built on top of the existing Lexa language compiler, and enriches the compiler with an implementation for effect handlers that reduces the runtime overhead for infrequent effects, adhering to zero-cost principle. Programmers can specify in code whether to use the zero-cost implementation or stock-lexa implementation for each effect.

The compiler implementation consists of following components:
- **Compiler Driver**: `./lexa` is the main entry point for the compiler, which orchestrates the compilation process.
- **Frontend**: The frontend typechecks the source code and translates it into C. This is implemented in OCaml and is located in `./src/sl`, `./src/lib`, and `./src/bin`.
- **Backend**: Clang is used to compile the generated C code into an executable. Several LLVM passes are used during this process, and they are located in `./passes`.
- **Runtime**: The semantics of effect handlers are implemented via stack switching, using assembly trampolines defined in `./src/stacktrek/stacktrek.h`.^[Described in detail in a prior OOPSLA’24 publication.] The stackwalker introduced in this work is located in `./src/stackwalker/stackwalker.c`.

## Claims
We made the following claims in the OOPSLA 2025 submission:
### Claim 1
> Zero-cost Lexa is faster than stock Lexa in almost all benchmarks. (line 1008 & Table 1)
### Claim 2
> As the time slice increases(and effects are raised less frequently), the performance advantage shifts from stock Lexa to zero-cost Lexa. (line 1061 & Figure 12)

# Hardware Dependencies
* Supported platform: x86-64
* 32GB of RAM & 4 CPU cores
* Linux operating system
* Root privilege is needed to turn off ASLR (Address Space Layout Randomization)

# Getting started(30 min)
1. **Turn off ASLR**: 
   - Run `echo 0 | sudo tee /proc/sys/kernel/randomize_va_space` to disable ASLR. Note that this setting has no impact on the performance results reported in the paper.
2. **Run Docker Container**: 
   - Install Docker on your system.
   - `docker pull hflsmax/lexa-lang:OOPSLA25`: Pull the Docker image.
   - `docker run -it hflsmax/lexa-lang:OOPSLA25`: Run the container interactively.
3. **Do A Quick Run**: 
   - `cd /WorkDir`: Enter root directory.
   - `cd scripts; python bench.py --systems lexa lexaz --benchmarks catalan --quick`: Build and bench a single benchmark program
   - `cat ./runtimes.csv`: Check that the results in `runtimes.csv`.
   - You should see something like this:
```
bash-5.2# cat runtimes.csv
,mean_mili,mean_mili,std_mili,std_mili
platform,lexa,lexaz,lexa,lexaz
benchmark,,,,
catalan,319.0,309.0,0.0,2.0
```

# Step by Step Instructions(30 min)
1. **Set up the environment**: Repeat step 1-2 from the "Getting started" section.
2. **Reproduce Claim 1**: 
   - `cd /WorkDir`: Enter root directory
   - `cd scripts; python bench.py --systems lexa lexaz --all-zero-cost-benchmarks`: Run the benchmarks for all zero-cost benchmarks.
   - `cat ./runtimes.csv`: Check that the results in `runtimes.csv`.
   - You should see the runtimes of `lexaz`(the zero-cost implementation) and `lexa`(the stock implementation) for all benchmarks. Most benchmarks should show that `lexaz` is faster than `lexa`. 
   - Due to the hardware differences and statistical noise, you may see some benchmarks where `lexa` is faster than `lexaz`. You can use the standard deviation to assess the statistical significance of these results. Our claim is that "Zero-cost Lexa is faster than stock Lexa in *almost* all benchmarks.".
3. **Reproduce Claim 2**:
    - `cd /WorkDir`: Enter root directory
    - `cd scripts/OOPSLA25; python ./plots.py`: Generate the plot for the two threads figure.
    - `docker cp CONTAINER_ID:/WorkDir/scripts/OOPSLA25/two-threads.pdf .`: Copy the plot from the container to your local machine, replacing `CONTAINER_ID` with the actual ID of the running container, which you can find by running `docker ps`.
    - You should see a plot similar to Figure 12 in the paper, showing that as the time slice increases, the performance advantage shifts from stock Lexa to zero-cost Lexa.
    - Due to the hardware differences, you may not see two lines crossing with the range of x-axis we used in the paper. You can adjust the x-axis range, via `vim ./plots.py`(line 99).

# Reusability Guide
Reusing this artifact means that you can use the Lexa compiler to compile code that you write. The surface syntax of Lexa should be familiar to programmers who have experience with functional programming languages like OCaml or Koka. Zero-cost and stock implementations of effect is distinguished using the keyword `exceptional effect` and `effect` at the effect definition, respectively. To get familiar with Lexa, we recommend studying the following short programs, which are ordered from simple to complex. For each example, we highlight the relevant syntax.
- `./benchmarks/lexa/countdown/main.lx`: A benchmark program that uses *stock Lexa* to implement a countdown.
   - Line 1: the effect is declared using `effect` keyword, signifying that it uses the stock Lexa implementation.
   - Line 21, 25: the handler is defined using `def` keyword, signifying that it is a tail-resumptive handler.
   - Line 19, 6, 7, 11: The handler labels(ie `state_stub`) are passed down as arguments explicitly
- `./benchmarks/lexaz/catalan/main.lx`: A benchmark program that uses *zero-cost Lexa* to implement an exception handler.
   - Line 30: the handler is defined using `exc` keyword, signifying that it is an abortive handler.
- `./test/lexa_snippets/mixed_impl/mixed_impl.lx`: A program that *mixes* zero-cost and stock implementations in a nested manner.
   - Line 11: the handler labels that are captured(ie `e1`) by an inner scope need to be explicitly stated

We now invite you to write your own Lexa code that uses effect handlers. Your goal is to rewrite the `countdown` program(`./benchmarks/lexa/countdown/main.lx`) so that when the count reaches zero(line 9), it raises an exception that is handled by a zero-cost effect handler. The exception handler shoud abort the countdown. We provide a sample solution `solution.lx` under the same directory.

Follow these steps to write and compile your own Lexa code:
1. **Set up the environment**: Follow the instructions in the "Getting started" section to set up the environment.
2. **Write your code**: 
   - The Docker image has `vim` and `emacs` installed.
   - `cd /WorkDir/benchmarks/lexa/countdown`: Navigate to the countdown benchmark directory.
   - `vim main.lx`: Edit `main.lx`.
3. **Compile your code**: 
   - `lexa main.lx -o main`: This will compile your code `main.lx` into an executable named `main`.
4. **Run your code**:
    - `./main 42`: This will run your compiled code.