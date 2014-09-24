#!/bin/sh
gitroot=$(git rev-parse --show-toplevel)
scala -classpath $gitroot/build/:$gitroot/vendor/antlr.jar compile.Compiler "$@"
