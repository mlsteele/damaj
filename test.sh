#!/bin/sh
run_parser() {
  $(git rev-parse --show-toplevel)/run.sh -t scan `pwd`/$1
}

test_parser() {
  outfile=`mktemp`
  expectfile=tests/scanner/output/$1.out
  run_parser tests/scanner/input/$1 2>&1 > $outfile

  echo "-- EXPECTED --"
  cat $expectfile
  echo "-- OUTPUT --"
  cat $outfile
  echo "-- DIFF --"
  diff $outfile $expectfile
  diffret=$?
  echo "-- END --"

  rm $outfile

  if [ $diffret -eq 0 ]; then
    echo "Test passed"
  else
    echo "Test FAILED"
  fi
}

test_parser $1
