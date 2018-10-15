SML := sml
SMLFLAGS := -Cprint.depth=10 -m
MLTON := mlton

CM_FILE := parsimony.cm
MLB_TEST_FILE := parsimony-test.mlb
TEST_BIN := parsimony-test

compile:
	$(SML) $(SMLFLAGS) $(CM_FILE)

tests: $(MLB_TEST_FILE)
	$(MLTON) $(MLB_TEST_FILE)
	./$(TEST_BIN)

examples:
	$(SML) $(SMLFLAGS) $(CM_FILE) example/infix.sml

clean:
	rm $(TEST_BIN)
