#!/bin/sh
if command -v add >/dev/null 2>&1; 
then
    add -f scala
fi
gitroot=$(git rev-parse --show-toplevel)
scala -classpath $gitroot/build/:$gitroot/vendor/antlr.jar compile.Compiler -d "$@"
dot -Tsvg tmp/unoptimized -o tmp/unoptimized.svg
dot -Tpng tmp/unoptimized -o tmp/unoptimized.png
