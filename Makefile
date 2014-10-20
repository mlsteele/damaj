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
ANTLR_CLASSES := $(patsubst %.g, %.class, $(patsubst src/grammars/%, build/src/grammars/%, $(ANTLR_SOURCES)))
JAVA_CLASSES := $(patsubst %.java, %.class, $(patsubst src/java/%, build/src/%, $(JAVA_SOURCES)))
# SCALA_CLASSES := $(patsubst %.scala, %.class, $(patsubst src/scala/%, build/src/%, $(SCALA_SOURCES)))


# Compile a .class file from a .java file
build/src/%.class: src/java/%.java
	@mkdir -p build
	javac -cp $(CLASSPATH) $< -d build

# Generate the .java file for a .g file
build/src/grammars/%.java: src/grammars/%.g 
	@mkdir -p build
	java -cp $(CLASSPATH) org.antlr.Tool -o build $< -debug

# Compile a .class file from a ANTLR-generated .java file
build/src/grammars/%.class: build/src/grammars/%.java
	@mkdir -p build
	javac -cp $(CLASSPATH) -d build $<

# Compile all .java files to .class
.PHONY:
java_classes: $(JAVA_CLASSES)

# .PHONY:
# scala_classes: $(SCALA_CLASSES)

# Compile all scala files together.
.PHONY:
scala_classes: $(JAVA_CLASSES) $(ANTLR_CLASSES)
	@mkdir -p build
	@echo "===============SCALA COMPILATION==============="
	@fsc $(SCALAC_FLAGS) -d build $(SCALA_SOURCES)
	@echo "===========SCALA COMPILATION COMPLETE=========="

.PHONY:
antlr_classes: $(ANTLR_CLASSES)

# Clear fsc cache
.PHONY:
fsc_reset:
	fsc -reset

# Generate documentation from .txt files using emacs' org-mode
doc/%.pdf: doc/%.txt
	emacs -q -file $< --batch -f org-mode -f org-export-as-pdf

.PHONY:
docs: doc/semantic_checker/Lab2.pdf	doc/codegen/CodeGeneration.pdf

# Runs the scala REPL with our files added to the path
.PHONY:
repl:
	CLASSPATH=$(CLASSPATH) scala -deprecation -feature -language:implicitConversions

.PHONY:
clean:
	fsc -reset
	rm -rf build/*

# Magically print any Makefile variable from the command line
print-%:
	@echo '$*=$($*)'
	@echo '  origin = $(origin $*)'
	@echo '  flavor = $(flavor $*)'
	@echo '   value = $(value  $*)'
