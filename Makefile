SML := sml
SMLFLAGS := -Cprint.depth=30

MLTON := mlton

CM_FILE := austral.cm
MLB_FILE := austral.mlb
MLB_EXE_FILE := austral-exe.mlb

CM_TEST_FILE := austral-test.cm
MLB_TEST_FILE := austral-test.mlb
TEST_BIN := austral-test

BIN = austral

C_RUNTIME_SRC := src/runtime.c
C_RUNTIME_SCRIPT := runtime.awk
C_RUNTIME_ML := src/c-runtime.sml
SRC := src/*.sig src/*.sml $(C_RUNTIME_ML)
TEST_SRC := test/*.sml

all: compile

$(C_RUNTIME_ML): $(C_RUNTIME_SRC) $(C_RUNTIME_SCRIPT)
	awk -f $(C_RUNTIME_SCRIPT) $(C_RUNTIME_SRC) > $(C_RUNTIME_ML)

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

.PHONY: docs
docs: $(DOCS_HTML)

clean:
	if [ -f $(BIN) ]; then rm $(BIN); fi
	if [ -f $(TEST_BIN) ]; then rm $(TEST_BIN); fi
	if [ -f $(C_RUNTIME_ML) ]; then rm $(C_RUNTIME_ML); fi
	if [ -f $(DOCS_HTML) ]; then rm $(DOCS_HTML); fi
	if [ -f $(DOCS_ARCH_PNG) ]; then rm $(DOCS_ARCH_PNG); fi
	if [ -d src/.cm/ ]; then rm -rf src/.cm/; fi
	rm -f test/valid/*.c
	rm -f test/valid/*.bin
