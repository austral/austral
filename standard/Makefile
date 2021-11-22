AUSTRAL := austral
CPP := g++


MODULES := --module=src/Box \
	--module=src/Buffer \
	--module=src/Console \
	--module=test/Unit \
	--module=test/Box_Test \
	--module=test/Buffer_Test \
	--module=test/Main

TEST_BIN := test_bin

.DEFAULT: run-tests

run-tests: $(TEST_BIN)
	./$(TEST_BIN)

$(TEST_BIN): src/*.aui src/*.aum test/*.aui test/*.aum
	$(AUSTRAL) compile $(MODULES) --entrypoint=Standard.Test:Main --output=test.cpp
	$(CPP) -std=c++11 test.cpp -o $(TEST_BIN)
	rm test.cpp

clean:
	rm $(TEST_BIN)
