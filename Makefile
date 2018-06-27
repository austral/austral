SML := sml
SMLFLAGS := -Cprint.depth=10

MLTON := mlton

CM_FILE := boreal.cm
MLB_FILE := boreal.mlb

MLB_TEST_FILE := boreal-test.mlb
TEST_BIN := boreal-test

SRC := src/*.sig src/*.sml
TEST_SRC := test/*.sml

VENDOR_DIR := vendor
MLUNIT := $(VENDOR_DIR)/mlunit
MLUNIT_URL := https://github.com/eudoxia0/mlunit.git

all: compile

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(MLUNIT): $(VENDOR_DIR)
	git clone $(MLUNIT_URL) $(MLUNIT)

compile: $(SRC)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(TEST_BIN): $(SRC) $(TEST_SRC) $(MLUNIT)
	$(MLTON) $(MLB_TEST_FILE)
	./$(TEST_BIN)

test: $(TEST_BIN)

clean:
	rm -rf $(VENDOR_DIR)
	rm $(TEST_BIN)
