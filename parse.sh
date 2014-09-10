#!/usr/bin/env bash

runscanner() {
  $(git rev-parse --show-toplevel)/run.sh -t scan `pwd`/$1
}

runparser() {
  $(git rev-parse --show-toplevel)/run.sh -t parse --debug `pwd`/$1
}


infile=$1
echo "-- TOKENS --"
runscanner $infile
echo "-- INPUT --"
cat $infile
runparser $infile
