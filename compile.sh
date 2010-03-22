#!/bin/sh 
set -e

rm -rf build || true
mkdir build
cp *.hs *.hsc *.c *.h build
cd build

#hsc2hs -I/usr/include/SDL/ *.hsc
gcc -c -I/usr/include/ -O3 -Wall foreignPollution.c #-O0 -ggdb #-pipe -time
ghc -c -Wall --make Main.hs "$@"
echo 'Linking...'
#no -lSDL
ghc -o game -package clock -package GLUT -lGL -lGLU -lglut -lm *.o "$@"

