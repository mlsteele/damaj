#!/bin/sh
if command -v add >/dev/null 2>&1; 
then
    add -f scala
fi
make all
