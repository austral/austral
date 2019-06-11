SML := sml
SMLFLAGS := -Cprint.depth=30

MLTON := mlton

CM_FILE := austral.cm
MLB_FILE := austral.mlb
CM_TEST_FILE := austral-test.cm

BIN = austral

C_RUNTIME_SRC := src/back/runtime.c
C_RUNTIME_SCRIPT := src/back/runtime.awk
C_RUNTIME_ML := src/back/c-runtime.sml
SRC := src/util/*.sig src/util/*.sml \
       src/front/*.sig src/front/*.sml \
       $(C_RUNTIME_ML)
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

clean:
	if [ -f $(BIN) ]; then rm $(BIN); fi
	if [ -f $(C_RUNTIME_ML) ]; then rm $(C_RUNTIME_ML); fi
	if [ -d src/util/.cm/ ]; then rm -rf src/util/.cm/; fi
	if [ -d src/front/.cm/ ]; then rm -rf src/front/.cm/; fi
	rm -f test/valid/*.c
	rm -f test/valid/*.bin
