# The DAMAJ Compiler

A really decent Decaf compiler.

"Mad decent" - Jess

### Etymology
Diony, Andres, Miles, And Jess's Compiler
Dainty airplanes make awful jets Compiler
Don't Always Make A Java Compiler

## Building
To build, run `make`. It will recompile the changed files.

You can also run `./build.sh` to trigger `make`.

## Running
To run the whole scriptadoodle:
```
./run.sh -t parse --debug tests/parser/legal/legal-01
```

Target can be `scan`, `parse`, or `inter`.
