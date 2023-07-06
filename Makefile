SOURCE_FILE :=

OUTPUT_FILE := $(notdir $(basename $(SOURCE_FILE)))

COMPILER := ghc

COMPILER_FLAGS := -o $(OUTPUT_FILE)

all: compile

compile: $(SOURCE_FILE)
	$(COMPILER) $(COMPILER_FLAGS) $(SOURCE_FILE)

run: compile
	./$(OUTPUT_FILE)

clean:
	mv -f $(OUTPUT_FILE) $(OUTPUT_FILE).o $(OUTPUT_FILE).hi out/



