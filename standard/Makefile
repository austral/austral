MODULES := \
    src/Tuples.aui,src/Tuples.aum \
	src/Bounded.aui,src/Bounded.aum \
    src/Equality.aui,src/Equality.aum \
	src/Box.aui,src/Box.aum \
	src/Buffer.aui,src/Buffer.aum \
	test/Unit.aui,test/Unit.aum \
	test/Tuples.aui,test/Tuples.aum \
	test/Bounded.aui,test/Bounded.aum \
	test/Equality.aui,test/Equality.aum \
	test/Box.aui,test/Box.aum \
	test/Buffer.aui,test/Buffer.aum \
	test/Main.aui,test/Main.aum
TEST_BIN := test_bin

.DEFAULT: all
all: $(TEST_BIN)

$(TEST_BIN): src/*.aui src/*.aum test/*.aui test/*.aum
	austral compile $(MODULES) --entrypoint=Standard.Test:Main --output=$(TEST_BIN)

clean:
	rm $(TEST_BIN)
