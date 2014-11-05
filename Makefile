.PHONY: java_classes scala_classes antlr_classes fsc_reset docs repl clean
all: fsc_reset scala_classes

CLASSPATH = build:vendor/antlr.jar
SCALAC_FLAGS = -cp $(CLASSPATH) -deprecation -feature -language:implicitConversions -Xlint $(WARNINGS)
WARNINGS = \
	-Ywarn-dead-code \
	-Ywarn-inaccessible \
	-Ywarn-infer-any \
	-Ywarn-nullary-override \
	-Ywarn-nullary-unit \
	-Ywarn-numeric-widen \
	-Ywarn-unused \
	-Ywarn-unused-import \
	-Ywarn-value-discard

ANTLR_SOURCES := src/grammars/DecafScanner.g src/grammars/DecafParser.g
#Gathers up .java files found under the java/ directory
JAVA_SOURCES := $(shell find src -name "*.java")
#Gathers up .java files found under the scala/ directory
SCALA_SOURCES := $(shell find src -name "*.scala")

# Generates .class file names while preserving subdirectories
ANTLR_CLASSES := $(patsubst %.g, %.class, $(patsubst src/grammars/%, build/grammars/%, $(ANTLR_SOURCES)))
JAVA_CLASSES := $(patsubst %.java, %.class, $(patsubst src/java/%, build/%, $(JAVA_SOURCES)))
# SCALA_CLASSES := $(patsubst %.scala, %.class, $(patsubst src/scala/%, build/src/%, $(SCALA_SOURCES)))


# Compile a .class file from a .java file
build/%.class: src/java/%.java
	@mkdir -p build tmp
	javac -cp $(CLASSPATH) $< -d build

# Generate the .java file for a .g file
build/src/grammars/%.java: src/grammars/%.g 
	@mkdir -p build tmp
	java -cp $(CLASSPATH) org.antlr.Tool -o build $< -debug

# Compile a .class file from a ANTLR-generated .java file
build/grammars/%.class: build/src/grammars/%.java
	@mkdir -p build tmp
	javac -cp $(CLASSPATH) -d build $<

# Compile all .java files to .class
java_classes: $(JAVA_CLASSES)

# scala_classes: $(SCALA_CLASSES)

# Compile all scala files together.
scala_classes: $(JAVA_CLASSES) $(ANTLR_CLASSES)
	@mkdir -p build tmp
	@echo "===============SCALA COMPILATION==============="
	@fsc $(SCALAC_FLAGS) -d build $(SCALA_SOURCES)
	@echo "===========SCALA COMPILATION COMPLETE=========="

antlr_classes: $(ANTLR_CLASSES)

# Clear fsc cache
fsc_reset:
	fsc -reset

# Generate documentation from .txt files using emacs' org-mode
doc/%.pdf: doc/%.txt
	emacs -q -file $< --batch -f org-mode -f org-export-as-pdf

docs: doc/semantic_checker/Lab2.pdf	doc/codegen/CodeGeneration.pdf

# Runs the scala REPL with our files added to the path
repl:
	CLASSPATH=$(CLASSPATH) scala -deprecation -feature -language:implicitConversions

clean:
	fsc -reset
	rm -rf build/*

# Magically print any Makefile variable from the command line
print-%:
	@echo '$*=$($*)'
	@echo '  origin = $(origin $*)'
	@echo '  flavor = $(flavor $*)'
	@echo '   value = $(value  $*)'
