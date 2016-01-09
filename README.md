# Minsky
Simple emulator for Minsky's register machines

## Features
* Emulate register machines with supplied configuration `conf` and program     `prog`
````
\*Executor> execute conf prog
````

* Translate binary code to Haskell data structures using `codeToProgram`
* Translate data structures to program code
````
\*Translator> codeToProgram 1234
[ (L0,R0+ -> L0)
, (L1,R0- -> L0, L0)
, (L2,R0+ -> L0)
, (L3,HALT)
, (L4,R0- -> L0, L0) ]
````

* Parse code from source file
````
$ ./bin/main examples/exampleProg.mnsk
fromList [(L0,R1- -> L1, L7),(L1,R0+ -> L2),(L2,R2- -> L3, L5),(L3,R3+ -> L4),(L4,R0+ -> L1),(L5,R2+ -> L6),(L6,R3- -> L5, L0),(L7,HALT)]
````

## TODO
* Read configuration from stdin, execute program accordingly
* Parse gadgets
* Graphical representation of programs (LaTex)
* Implement universal register machine as an example
* Execute program step by step
