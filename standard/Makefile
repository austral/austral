MODULES := --module=src/Tuples.aui,src/Tuples.aum \
	--module=src/Bounded.aui,src/Bounded.aum \
	--module=test/Unit.aui,test/Unit.aum \
	--module=test/Tuples.aui,test/Tuples.aum \
	--module=test/Bounded.aui,test/Bounded.aum \
	--module=test/Main.aui,test/Main.aum
TEST_BIN := test_bin

.DEFAULT: all
all: $(TEST_BIN)

$(TEST_BIN): src/*.aui src/*.aum test/*.aui test/*.aum
	austral compile $(MODULES) --entrypoint=Standard.Test:Main --output=test.c
	gcc -fwrapv test.c -o $(TEST_BIN)
	rm test.c

clean:
	rm $(TEST_BIN)
