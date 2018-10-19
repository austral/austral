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

CPP_RUNTIME_ML := src/cpp-runtime.sml
SRC := src/*.sig src/*.sml $(CPP_PRELUDE_ML)
TEST_SRC := test/*.sml

all: compile

$(CPP_PRELUDE_ML): src/runtime.hpp runtime.awk
	awk -f runtime.awk src/runtime.hpp > $(CPP_PRELUDE_ML)

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
	if [ -f boreal ]; then rm $(BIN); fi
	if [ -f boreal ]; then rm $(TEST_BIN); fi
	rm $(CPP_RUNTIME_ML)
	rm test/valid/*.cpp
