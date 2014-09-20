.PHONY: all build run
CLASSPATH=build/lib/project.jar:lib/antlr.jar

all: grammar build

# build:
	# ANT_HOME=/usr/share/ant SCALA_HOME=/usr/share/java ant

build:
	mkdir -p build
	@echo "HOW DO I BUILD?"

run:
	scala -classpath $(CLASSPATH) compile.Compiler $(args) --target=scan tests/scanner/input/char1

grammars: scanner

scanner:
	java -cp parser/lib/antlr.jar org.antlr.Tool -fo parser/autogen/edu/mit/compilers/grammar/ parser/src/edu/mit/compilers/grammar/DecafScanner.g
