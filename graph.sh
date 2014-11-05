#!/bin/sh
if command -v add >/dev/null 2>&1; 
then
    add -f scala
fi
gitroot=$(git rev-parse --show-toplevel)
scala -classpath $gitroot/build/:$gitroot/vendor/antlr.jar compile.Compiler -d "$@"
for file in `dirname $0`/tmp/*.gv; do
  echo "rendering graph: $file to .svg, .png"
  dot -Tsvg $file -o $file.svg
  dot -Tpng $file -o $file.png
done
