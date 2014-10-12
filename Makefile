all: fsc_reset scala_classes

CLASSPATH = build:vendor/antlr.jar

ANTLR_SOURCES := grammars/DecafScanner.g grammars/DecafParser.g
#Gathers up .java files found under the java/ directory
JAVA_SOURCES := $(shell find java -name "*.java")
#Gathers up .java files found under the scala/ directory
SCALA_SOURCES := $(shell find scala -name "*.scala")

# Generates .class file names while preserving subdirectories
ANTLR_CLASSES := $(patsubst %.g, %.class, $(patsubst grammars/%, build/grammars/%, $(ANTLR_SOURCES)))
JAVA_CLASSES := $(patsubst %.java, %.class, $(patsubst java/%, build/%, $(JAVA_SOURCES)))
# SCALA_CLASSES := $(patsubst %.scala, %.class, $(patsubst scala/%, build/%, $(SCALA_SOURCES)))


# Compile a .class file from a .java file
build/%.class: java/%.java
	@mkdir -p build
	javac -cp $(CLASSPATH) $< -d build

# Generate the .java file for a .g file
build/grammars/%.java: grammars/%.g 
	@mkdir -p build
	java -cp $(CLASSPATH) org.antlr.Tool -o build $< -debug

# Compile a .class file from a ANTLR-generated .java file
build/grammars/%.class: build/grammars/%.java
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
	fsc -deprecation -feature -language:implicitConversions -cp $(CLASSPATH) -d build $(SCALA_SOURCES)

.PHONY:
antlr_classes: $(ANTLR_CLASSES)

# Clear fsc cache
.PHONY:
fsc_reset:
	fsc -reset

# Runs the scala REPL with our files added to the path
.PHONY:
repl:
	CLASSPATH=$(CLASSPATH) scala -deprecation -feature -language:implicitConversio

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
