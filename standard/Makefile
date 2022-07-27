MODULES := --module=src/Tuples \
	--module=test/Unit \
	--module=test/Main
TEST_BIN := test_bin

.DEFAULT: all
all: $(TEST_BIN)

$(TEST_BIN): src/*.aui src/*.aum test/*.aui test/*.aum
	austral compile $(MODULES) --entrypoint=Standard.Test:Main --output=test.c
	gcc -fwrapv test.c -o $(TEST_BIN)
	rm test.c

clean:
	rm $(TEST_BIN)
