This is a randomly generated world simulation.  It has machines that make
energy bolts but also emit smog, and some other natural and peculiar things.

To build, you need a Haskell development environment (Haskell Platform,
or GHC + cabal-install, are fine), as well as the OpenGL and GLUT C libraries.
Then,

`cabal configure; cabal build` (if you have this source package. You might
have to follow Cabal's instructions to install packages this depends on, then
try again)
  or
`cabal install Pollutocracy` (if you want to download and install the version
online on Hackage; the binary might then be ~/.cabal/bin/Pollutocracy).

I've only used it on Linux myself; if you're lucky it might work on another
system too.

To run the simulation faster, run with the command-line argument -s N, e.g.
`-s 100`, where 500 is the default value and lower is faster.

