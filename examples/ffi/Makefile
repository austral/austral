STDLIB := \
		../../standard/src/Tuples.aui,../../standard/src/Tuples.aum \
		../../standard/src/Bounded.aui,../../standard/src/Bounded.aum \
		../../standard/src/Equality.aui,../../standard/src/Equality.aum \
		../../standard/src/Order.aui,../../standard/src/Order.aum \
		../../standard/src/Box.aui,../../standard/src/Box.aum \
		../../standard/src/Buffer.aui,../../standard/src/Buffer.aum \
		../../standard/src/String.aui,../../standard/src/String.aum \
		../../standard/src/StringBuilder.aui,../../standard/src/StringBuilder.aum \
		../../standard/src/IO/IO.aui,../../standard/src/IO/IO.aum \
		../../standard/src/IO/Terminal.aui,../../standard/src/IO/Terminal.aum \
		
PROG := FFI

ENTRY := Example.$(PROG):main

SRC := \
	$(PROG).aui,$(PROG).aum

.DEFAULT: all
all: 
	../../austral compile $(STDLIB) $(SRC) --entrypoint=$(ENTRY) --output=main

.PHONY: clean
clean:
	rm main
