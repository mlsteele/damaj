#!/bin/bash

runcompiler() {
    $(git rev-parse --show-toplevel)/run.sh --target=assembly -o $2 $1
}

compile() {
    gcc -o $2 -Ltests/optimizer/lib $1 -l6035 -lpthread
}

if [[ -z "$1" ]]; then
    echo "run with an input decaf program to compile"
    exit
fi

root=$(git rev-parse --show-toplevel)
asm="$root/tmp/out.s"
binary="$root/tmp/out"
mkdir -p "$root/tmp"

runcompiler "$*" $asm &&\
echo "$asm" &&\
cat "$asm" &&\
compile $asm $binary &&\
echo "$binary" &&\
tmp/out
