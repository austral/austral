SML := sml
SMLFLAGS := -Cprint.depth=10

MLTON := mlton

CM_FILE := boreal.cm
MLB_FILE := boreal.mlb

CM_TEST_FILE := boreal-test.cm
TEST_BIN := boreal-test

BIN = boreal

SRC := src/*.sig src/*.sml
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

compile: $(SRC) $(DEPS)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(BIN): $(SRC) $(DEPS)
	$(MLTON) $(MLB_FILE)

$(TEST_BIN): $(SRC) $(TEST_SRC) $(DEPS)
	$(SML) $(SMLFLAGS) -m $(CM_TEST_FILE)

.PHONY: test
test: $(TEST_BIN)

clean:
	rm -rf $(VENDOR_DIR)
	rm $(TEST_BIN)
