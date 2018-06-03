SML := sml
SMLFLAGS := -Cprint.depth=10

CM_FILE := boreal.cm

SRC := src/*.sig src/*.sml

all: compile

compile: $(SRC)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)
