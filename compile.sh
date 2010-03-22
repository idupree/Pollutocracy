#!/bin/sh 
set -e

#instead of this, let's try letting it not recompile sometimes
#rm -rf build || true
#mkdir build
#cp
#rsync -u doesn't update files whose modification-times haven't changed
rsync -u *.hs *.hsc *.c *.h build
#although, if you want the warnings again, or different minor compile options,
#hmm...
cd build

#hsc2hs -I/usr/include/SDL/ *.hsc
gcc -c -I/usr/include/ -O3 -Wall foreignPollution.c #-O0 -ggdb #-pipe -time
ghc -c -Wall --make Main.hs "$@"
echo 'Linking...'
#no -lSDL
ghc -o game -package clock -package GLUT -lGL -lGLU -lglut -lm *.o "$@"

