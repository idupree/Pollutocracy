#!/bin/sh 
set -e

# To build this game, you need
# * a Haskell development environment, namely, GHC
# * the C GLUT & OpenGL libraries
# * the Haskell 'clock' and 'GLUT' libraries, which can be installed
#     by 'cabal install clock' and 'cabal install GLUT' if you have
#     a recent Haskell Platform.
# * Also you need a bit of POSIX, because the 'clock' library depends on it.
#     I know Linux works, I'm sure Mac OS X does, and maybe MinGW
#     or even plain Windows does...I haven't tried it.
# * I haven't tried it outside of the GCC / Linux world, so it might just
#     not work elsewhere, though it "should".
#
# Then run ./compile.sh and hope it works!


#instead always deleting/redoing the build directory
#rm -rf build || true; mkdir build; cp ...,
#rsync -u doesn't update files whose modification-times haven't changed
rsync -u *.hs *.c *.h build
  #*.hsc commented since there are no .hsc files anymore
#although, if you want the warnings again, or different minor compile options,
#hmm...
cd build

#we don't use SDL anymore
#hsc2hs -I/usr/include/SDL/ *.hsc
gcc -c -I/usr/include/ -O3 -Wall foreignPollution.c #-O0 -ggdb #-pipe -time
ghc -c -Wall --make Main.hs "$@"
echo 'Linking...'
#no -lSDL
ghc -o game -package clock -package GLUT -lGL -lGLU -lglut -lm *.o "$@"

