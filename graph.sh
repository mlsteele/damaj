#!/bin/sh
if command -v add >/dev/null 2>&1; 
then
    add -f scala
fi
gitroot=$(git rev-parse --show-toplevel)
scala -classpath $gitroot/build/:$gitroot/vendor/antlr.jar compile.Compiler -d "$@"
dot -Tsvg tmp/graph.gv -o tmp/graph.svg
dot -Tpng tmp/graph.gv -o tmp/graph.png
