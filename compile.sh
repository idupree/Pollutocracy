#!/bin/sh 
set -e

# To build this game, you need
# * a Haskell development environment, namely, GHC
# * the C GLUT & OpenGL libraries
# * the Haskell 'clock' and 'GLUT' libraries, which can be installed
#     by 'cabal install clock' and 'cabal install GLUT' if you have
#     a recent Haskell Platform.
# * I haven't tried it outside of the GCC / Linux world, so it might just
#     not work elsewhere, though it "should".
#
# Then run ./compile.sh and hope it works!


# Make a separate build directory so that intermediate files don't
# clutter the main directory.  (This project should probably use Cabal
# instead of this script.  I started this project before Cabal existed(!).

# rsync of multiple files creates the dest dir if it doesn't exist already.
# rsync -u doesn't update files whose modification-times haven't changed.
rsync -u *.hs *.c *.h build
cd build

gcc -c -I/usr/include/ -O3 -Wall foreignPollution.c
ghc -c -Wall --make Main.hs "$@"
echo 'Linking...'
ghc -o game -package clock -package GLUT -package random -lGL -lGLU -lglut -lm *.o "$@"

