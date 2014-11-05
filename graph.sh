#!/bin/sh
if command -v add >/dev/null 2>&1; 
then
    add -f scala
fi
gitroot=$(git rev-parse --show-toplevel)

# remove old graphs
rm -vf tmp/*.gv
rm -vf tmp/*.gv.*

# generate .gv's
scala -classpath $gitroot/build/:$gitroot/vendor/antlr.jar compile.Compiler -d -o /dev/null "$@"

# generate .svg's
for file in `dirname $0`/tmp/*.gv; do
  echo "rendering graph: $file.svg"
  dot -Tsvg $file -o $file.svg
done
