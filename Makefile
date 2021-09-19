BIN := austral
SRC := lib/*.ml lib/*.mli lib/*.mll lib/*.mly lib/dune bin/dune bin/austral.ml

all: $(BIN)

$(BIN): $(SRC)
	dune build
	cp _build/default/bin/austral.exe $(BIN)

.PHONY: test
test: $(BIN)
	dune runtest

install: $(BIN)
	install -m 755 austral /usr/local/bin/austral

uninstall:
	sudo rm /usr/local/bin/austral

clean:
	rm $(BIN); rm -rf _build
