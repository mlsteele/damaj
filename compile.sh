#!/bin/bash

runcompiler() {
    $(git rev-parse --show-toplevel)/run.sh --opt=all --target=assembly -o $2 $1
}

compile() {
    gcc -o $2 $1
}

if [[ -z "$1" ]]; then
    echo "run with an input decaf program to compile"
    exit
fi

root=$(git rev-parse --show-toplevel)
asm="$root/tmp/out.S"
binary="$root/tmp/out"
mkdir -p "$root/tmp"

runcompiler "$1" $asm &&\
echo "$asm" &&\
cat "$asm" &&\
compile $asm $binary &&\
echo "$binary"
