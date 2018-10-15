SML := sml
SMLFLAGS := -Cprint.depth=30

MLTON := mlton

CM_FILE := boreal.cm
MLB_FILE := boreal.mlb
MLB_EXE_FILE := boreal-exe.mlb

CM_TEST_FILE := boreal-test.cm
MLB_TEST_FILE := boreal-test.mlb
TEST_BIN := boreal-test

BIN = boreal

CPP_PRELUDE_ML := src/cpp-prelude.sml
SRC := src/*.sig src/*.sml $(CPP_PRELUDE_ML)
TEST_SRC := test/*.sml

VENDOR_DIR := vendor
MLUNIT := $(VENDOR_DIR)/mlunit
MLUNIT_URL := https://github.com/eudoxia0/mlunit.git
PARSIMONY := $(VENDOR_DIR)/parsimony
PARSIMONY_URL := https://github.com/eudoxia0/parsimony.git
DEPS := $(MLUNIT) $(PARSIMONY)

all: compile

$(CPP_PRELUDE_ML): src/prelude.hpp prelude.awk
	awk -f prelude.awk src/prelude.hpp > $(CPP_PRELUDE_ML)

compile: $(SRC)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(BIN): $(SRC)
	$(MLTON) -output $(BIN) $(MLB_EXE_FILE)

.PHONY: test
test: $(SRC) $(TEST_SRC)
	$(SML) $(SMLFLAGS) -m $(CM_TEST_FILE)

$(TEST_BIN): $(SRC) $(TEST_SRC)
	$(MLTON) $(MLB_TEST_FILE)

.PHONY: mlton-test
mlton-test: $(TEST_BIN)
	./$(TEST_BIN)

clean:
	rm $(BIN)
	rm $(TEST_BIN)
	rm test/valid/*.cpp
