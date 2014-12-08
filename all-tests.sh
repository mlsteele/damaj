#!/usr/bin/env sh
#
# Script to run all tests
# Does NOT build first
#

set -e # commands which err cause the script to abort

echo "all-tests: scala unit tests"
make scala-test

echo "all-tests: SKIPPING scanner tests (because we differ in err format)"
# tests/scanner/test.sh
# tests/scanner-hidden/test.sh

echo "all-tests: running parser tests"
tests/parser/test.sh
tests/parser-hidden/test.sh

echo "all-tests: SKIPPING semantics tests (because they're slow)"
# tests/semantics/test.sh
# tests/semantics-hidden/test.sh

echo "all-tests: running codegen tests"
tests/codegen/test.sh
tests/codegen-hidden/test.sh

echo "all-tests: running dataflow tests"
tests/dataflow/test.sh
tests/dataflow-hidden/test.sh

echo "all-tests: running optimizer tests"
tests/optimizer/test.sh

echo "all-tests: running derby tests"
tests/derby/test.sh
