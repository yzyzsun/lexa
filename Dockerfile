FROM nixos/nix

WORKDIR /WorkDir
COPY ./flake.nix ./flake.lock .
COPY ./nix ./nix
RUN nix --extra-experimental-features "nix-command flakes" build --accept-flake-config --no-link .#clang_18_preserve_none --cores 16
RUN nix --extra-experimental-features "nix-command flakes" build --accept-flake-config --no-link nixpkgs#texliveSmall --cores 16
RUN nix --extra-experimental-features "nix-command flakes" build --accept-flake-config --no-link .#effect_latest --cores 16
RUN nix --extra-experimental-features "nix-command flakes" develop -j8 --cores 2
RUN nix --extra-experimental-features "nix-command flakes" develop --command bash -c "opam init --disable-sandboxing && eval $(opam env) && opam switch create -y 5.3.0 && opam install -y multicont"
RUN nix-env -iA nixpkgs.util-linux nixpkgs.time

COPY src src
COPY passes passes
COPY test test
COPY benchmarks benchmarks
COPY stdlib stdlib
COPY scripts scripts
COPY lexa lexa
COPY LICENSE LICENSE
COPY dune-project dune-project
COPY scripts/OOPSLA25/README.md README.md

RUN echo -e '#!/usr/bin/env bash\n\
nix --extra-experimental-features "nix-command flakes" --accept-flake-config develop' > /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]