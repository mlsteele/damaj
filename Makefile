.PHONY: all build build-dir grammars scanner java scala clean
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
	fsc -reset -d build/ -classpath build/:vendor/antlr.jar $(SCALA_FILES)

grammars: scanner

scanner:
	java -cp vendor/antlr.jar org.antlr.Tool -o build/ grammars/DecafScanner.g
	javac -cp vendor/antlr.jar -d build/ build/grammars/DecafScanner.java

clean:
	rm -rf build/*
