#!/bin/bash
BUILD=/home/tobias/code/tcc-project/build/knot/
BIN=/home/tobias/code/tcc-project/build/bin/knotc
SRC=/home/tobias/code/tcc-project/src/knot/

ghc "$SRC/Main.hs" -outputdir "$BUILD" -o "$BIN"


