#!/usr/bin/env sh
#
# Script to run all tests
# Does NOT build first
#

set -e # commands which err cause the script to abort

echo "all-tests: scala unit tests"
make scala-test

echo "all-tests: skipping scanner tests"
# tests/scanner/test.sh
# tests/scanner-hidden/test.sh

echo "all-tests: running parser tests"
tests/parser/test.sh
tests/parser-hidden/test.sh

echo "all-tests: running semantics tests"
tests/semantics/test.sh
tests/semantics-hidden/test.sh

echo "all-tests: running codegen tests"
tests/codegen/test.sh
tests/codegen-hidden/test.sh

echo "all-tests: running dataflow tests"
tests/dataflow/test.sh

echo "all-tests: running optimizer tests"
tests/optimizer/test.sh
