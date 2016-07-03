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

deps=$(
  cut -d: -f2 "$DIR/dependencies.txt" |
  xargs echo |
  tr " " "\n" |
  sort |
  uniq |
  # Exclude packages included in GHC
  grep -v 'base\|array\|containers\|time'
)

cabal update

echo "$deps" | xargs cabal install
