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

ghc-pkg list

# DO NOT MERGE: Try to clear that cache manually
rm -rf $HOME/.ghc
rm -rf $HOME/.cabal

ghc-pkg list

cabal update
# This is a fairly minimal set
cabal install \
      primitive \
      random \
      tf-random \
      HUnit \
      QuickCheck \
      split \
      text \
      bytestring \
      attoparsec \
      vector \
      parallel \
      stm \
      old-locale \
      lens
