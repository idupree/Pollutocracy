#!/bin/sh
ghc -package GLUT -lGL -lGLU -lglut -lSDL -fffi -I/usr/include/SDL/ -Wall --make Main.hs "$@"
