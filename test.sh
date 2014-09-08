#!/bin/sh
run_parser() {
  $(git rev-parse --show-toplevel)/run.sh -t scan `pwd`/$1
}

test_parser() {
  outfile=`mktemp`
  expectfile=tests/scanner/output/$1.out
  run_parser > $outfile 2>&1 tests/scanner/input/$1
  sed -i -e "s/\/home\/miles\/Documents\/15th_Grade\/6.035\/parser\/tests\/scanner\/input\///g" $outfile

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
