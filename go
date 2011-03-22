#!/bin/sh

ghc -O Main.hs -o cibberish && ./cibberish | clang -x c - && ./a.out && echo it works!
