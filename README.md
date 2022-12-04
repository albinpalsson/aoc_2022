# aoc_2022
Repository for Advent of Code 2022.
Code written in Haskell.

This year I put all setup and environment control into one script, _setup.sh_.
Running `./setup dayX` does the following:
* If directory doesnt already exist, initiates a new folder with skeleton code.
* In a forever running loop, triggered when any source code is modified:
  * format code
  * compile (linking with shared library)
  * run and measure time for executeable

Requires Bash, GHC and Ormolu (code formatter).
