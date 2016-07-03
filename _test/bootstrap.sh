#!/bin/bash
# This installs a the prerequisites for a given GHC/Cabal config
set -x
set -e
if [ ! -z "$GHCVER" ]; then
    export PATH=/opt/ghc/${GHCVER}/bin:$PATH
fi
if [ ! -z "$CABALVER" ]; then
    export PATH=/opt/cabal/${CABALVER}/bin:$PATH
fi

# What directory is this script in?
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

deps=$($DIR/dependencies $(ls $DIR/../exercises))
already_installed=$(ghc-pkg list --names-only --simple-output | tr " " "\n" | sort)
not_installed=$(comm -23 <(echo "$deps") <(echo "$already_installed"))

if [ -n "$not_installed" ]; then
  cabal update
  echo "$not_installed" | xargs cabal install
fi
