# Minsky
Simple emulator for Minsky's register machines

## Features
* Emulate register machines with supplied configuration `conf` and program     `prog`
````
\*Executor> execute conf prog
````

* Translate binary code for the universal register machine to Haskell data structures using `codeToProgram`
````
\*Translator> codeToProgram 1234
[ (L0,R0+ -> L0)
, (L1,R0- -> L0, L0)
, (L2,R0+ -> L0)
, (L3,HALT)
, (L4,R0- -> L0, L0) ]
````

## TODO
* Translate data structures to program code
* Parse code
* Parse gadgets
* Graphical representation of programs (LaTex)
* Implement universal register machine as an example
