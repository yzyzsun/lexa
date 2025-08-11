FROM nixos/nix

WORKDIR /WorkDir
COPY ./flake.nix ./flake.lock .
COPY ./nix ./nix
RUN nix --extra-experimental-features "nix-command flakes" --accept-flake-config develop .#userShell

COPY src src
COPY passes passes
COPY test test
COPY benchmarks benchmarks
COPY stdlib stdlib
COPY scripts scripts
COPY lexa lexa
COPY LICENSE LICENSE
COPY dune-project dune-project

RUN echo -e '#!/usr/bin/env bash\n\
nix --extra-experimental-features "nix-command flakes" --accept-flake-config develop .#userShell' > /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]