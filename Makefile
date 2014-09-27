all: scala_classes

CLASSPATH = build/lib/project.jar:vendor/antlr.jar:build

ANTLR_SOURCES := grammars/DecafScanner.g grammars/DecafParser.g
#Gathers up .java files found under the java/ directory
JAVA_SOURCES := $(shell find java -name "*.java")
#Gathers up .java files found under the scala/ directory
SCALA_SOURCES := $(shell find scala -name "*.scala")

# Generates .class file names while preserving subdirectories
ANTLR_CLASSES := $(patsubst %.g, %.class, $(patsubst grammars/%, build/grammars/%, $(ANTLR_SOURCES)))
JAVA_CLASSES := $(patsubst %.java, %.class, $(patsubst java/%, build/%, $(JAVA_SOURCES)))
SCALA_CLASSES := $(patsubst %.scala, %.class, $(patsubst scala/%, build/%, $(SCALA_SOURCES)))


# Compile a .class file from a .java file
build/%.class: java/%.java
	@mkdir -p build
	javac -cp $(CLASSPATH) $< -d build

# Compile a .class file from a .scala file
build/%.class: scala/%.scala $(JAVA_CLASSES) $(ANTLR_CLASSES)
	@mkdir -p build
	scalac -cp $(CLASSPATH) -d build $<

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

.PHONY:
scala_classes: $(SCALA_CLASSES)

.PHONY:
antlr_classes: $(ANTLR_CLASSES)

scala:
	fsc -reset -deprecation -d build/ -classpath build/:vendor/antlr.jar $(SCALA_FILES)

clean:
	rm -rf build/*

# Magically print any Makefile variable from the command line
print-%:
	@echo '$*=$($*)'
	@echo '  origin = $(origin $*)'
	@echo '  flavor = $(flavor $*)'
	@echo '   value = $(value  $*)'
