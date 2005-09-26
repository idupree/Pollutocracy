#!/bin/sh 
gcc -c -I/usr/include/ -O3 -pipe -time -Wall foreignPollution.c && \
 ghc -c -fffi -I/usr/include/SDL/ -Wall -fglasgow-exts --make Main.hs "$@" && \
 echo 'Linking...' && \
 ghc -package GLUT -lGL -lGLU -lglut -lSDL -lm *.o

