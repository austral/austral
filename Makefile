SML := sml
SMLFLAGS := -Cprint.depth=30

MLTON := mlton

CM_FILE := boreal.cm
MLB_FILE := boreal.mlb

CM_TEST_FILE := boreal-test.cm
MLB_TEST_FILE := boreal-test.mlb
TEST_BIN := boreal-test

BIN = boreal

SRC := src/*.sig src/*.sml src/cpp-prelude.sml
TEST_SRC := test/*.sml

VENDOR_DIR := vendor
MLUNIT := $(VENDOR_DIR)/mlunit
MLUNIT_URL := https://github.com/eudoxia0/mlunit.git
PARSIMONY := $(VENDOR_DIR)/parsimony
PARSIMONY_URL := https://github.com/eudoxia0/parsimony.git
DEPS := $(MLUNIT) $(PARSIMONY)

all: compile

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(MLUNIT): $(VENDOR_DIR)
	git clone $(MLUNIT_URL) $(MLUNIT)

$(PARSIMONY): $(VENDOR_DIR)
	git clone $(PARSIMONY_URL) $(PARSIMONY)

src/cpp-prelude.sml: src/prelude.hpp prelude.awk
	awk -f prelude.awk src/prelude.hpp > src/cpp-preludesml

compile: $(SRC) $(DEPS)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(BIN): $(SRC) $(DEPS)
	$(MLTON) $(MLB_FILE)

.PHONY: test
test: $(SRC) $(TEST_SRC) $(DEPS)
	$(SML) $(SMLFLAGS) -m $(CM_TEST_FILE)

$(TEST_BIN): $(SRC) $(TEST_SRC) $(DEPS)
	$(MLTON) $(MLB_TEST_FILE)

.PHONY: mlton-test
mlton-test: $(TEST_BIN)
	./$(TEST_BIN)

clean:
	rm -rf $(VENDOR_DIR)
	rm $(BIN)
	rm $(TEST_BIN)
