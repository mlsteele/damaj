# The DAMAJ Compiler

A really decent Decaf compiler.

"Mad decent" - Jess

See `doc/semantic_checker` for more serious documentation.

### Etymology
Diony, Andres, Miles, And Jess's Compiler
Dainty airplanes make awful jets Compiler
Don't Always Make A Java Compiler

## Building
Requires scala 2.11.2, please use `add -f scala` on Athena and see `doc/semantic_checker`
for more instructions.

To build, run `build.sh` or `make`. It will recompile the changed files.

## Running
To run the whole scriptadoodle:
```
./run.sh -t inter --debug tests/semantic/legal/legal-01
```

Target can be `scan`, `parse`, or `inter`.
