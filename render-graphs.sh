#!/bin/sh

# remove old RENDERED graphs
rm -vf tmp/*.gv.svg

for file in `dirname $0`/tmp/*.gv; do
  echo "rendering graph: $file.svg"
  dot -Tsvg $file -o $file.svg
done
