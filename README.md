# DAMAJ Decaf Compiler

A really decent Decaf compiler.

"Mad decent" - Jess

## Building
To build everything run `make`.

Oftentimes you will just edit scala, so run `make scala` to only recompile the scala.

To run the whole scriptadoodle:
```
./run.sh -t parse --debug tests/parser/legal/legal-01
```

Target can be `scan`, `parse`, or `inter`.
