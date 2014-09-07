.PHONY: all build run
CLASSPATH=build/lib/project.jar:lib/antlr.jar

all: build

build:
	SCALA_HOME=/usr/share/java ant

run:
	scala -classpath $(CLASSPATH) compile.Compiler $(args) --target=scan tests/scanner/input/char1
