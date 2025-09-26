# Simplified Haskell Interpreter

I implemented an interpreter for a subset of the Haskell language in Haskell. Like Haskell, it uses [combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic). The program outputs the reduction process step by step, which was the main difficulty of this project.

Syntax is composed of constants, constructors, functions and comments. A progam in this language is a list of functions, where one of the functions must be called `main` and have no arguments. 

This was a great opportunity for me to learn about functional programming concepts, such as immutability, higher order functions and monad transformers. This taught me how to design simple, reliable, readable and elegant solutions.

Run `przyk/run_tests.sh` (script supplied by the University of Warsaw) in project root directory to understand how the solution works. You need to have Cabal installed, see https://www.haskell.org/downloads/.
