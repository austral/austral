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

C_RUNTIME_SRC := src/runtime.c
C_RUNTIME_SCRIPT := runtime.awk
C_RUNTIME_ML := src/c-runtime.sml
SRC := src/*.sig src/*.sml $(C_RUNTIME_ML)
TEST_SRC := test/*.sml

DOCS_DIR := docs
DOCS_SRC := $(DOCS_DIR)/internals.md
DOCS_HTML := $(DOCS_DIR)/internals.html

DOCS_ARCH_SRC := $(DOCS_DIR)/architecture.mmd
DOCS_ARCH_PNG := $(DOCS_DIR)/architecture.png
MERMAID := ./node_modules/.bin/mmdc
MERMAID_P_CONFIG := $(DOCS_DIR)/puppeteer-config.json

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

$(DOCS_ARCH_PNG): $(DOCS_ARCH_SRC)
	$(MERMAID) -i $(DOCS_ARCH_SRC) -o $(DOCS_ARCH_PNG) -t neutral -p $(MERMAID_P_CONFIG)

$(DOCS_HTML): $(DOCS_SRC) $(DOCS_ARCH_PNG)
	pandoc $(DOCS_SRC) -f markdown+smart -t html -s -o $(DOCS_HTML)

clean:
	if [ -f $(BIN) ]; then rm $(BIN); fi
	if [ -f $(TEST_BIN) ]; then rm $(TEST_BIN); fi
	if [ -f $(C_RUNTIME_ML) ]; then rm $(C_RUNTIME_ML); fi
	if [ -f $(DOCS_HTML) ]; then rm $(DOCS_HTML); fi
	if [ -f $(DOCS_ARCH_PNG) ]; then rm $(DOCS_ARCH_PNG); fi
	rm test/valid/*.c
	rm test/valid/*.bin
