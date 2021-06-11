BIN := austral
SRC := lib/*.ml lib/*.mli lib/dune bin/dune bin/austral.ml

all: $(BIN)

$(BIN): $(SRC)
	dune build
	cp _build/default/bin/austral.exe $(BIN)

clean:
	rm $(BIN)
	rm -rf _build
