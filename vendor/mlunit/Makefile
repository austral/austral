SML := sml
SMLFLAGS := -Cprint.depth=10 -m
MLTON := mlton

CM_FILE := mlunit.cm
MLB_TEST_FILE := mlunit-test.mlb
TEST_BIN := mlunit-test

compile:
	$(SML) $(SMLFLAGS) $(CM_FILE)

tests:
	$(MLTON) $(MLB_TEST_FILE)
	./$(TEST_BIN)

clean:
	rm $(TEST_BIN)
