.PHONY: all build build-dir java scala grammars scanner parser clean
CLASSPATH=build/lib/project.jar:lib/antlr.jar
JAVA_FILES=`find java -type f -name "*.java"`
SCALA_FILES=`find scala -type f -name "*.scala"`

all: build

build: build-dir grammars java scala

build-dir:
	mkdir -p build

java:
	javac -d build/ $(JAVA_FILES)

scala:
	fsc -reset -deprecation -d build/ -classpath build/:vendor/antlr.jar $(SCALA_FILES)

grammars: scanner parser

scanner:
	java -cp vendor/antlr.jar org.antlr.Tool -o build/ grammars/DecafScanner.g -debug
	javac -cp vendor/antlr.jar -d build/ build/grammars/DecafScanner.java

parser:
	java -cp vendor/antlr.jar org.antlr.Tool -o build/ grammars/DecafParser.g -debug
	javac -cp vendor/antlr.jar -d build/ build/grammars/DecafParser.java

clean:
	rm -rf build/*
