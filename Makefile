BIN := austral
SRC := lib/*.ml lib/*.mli lib/*.mll lib/*.mly lib/dune bin/dune bin/austral.ml lib/BuiltInModules.ml

all: $(BIN)

lib/BuiltInModules.ml: lib/builtin/*.aui lib/builtin/*.aum
	python3 concat_builtins.py

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
	rm $(BIN); rm -rf _build; rm lib/BuiltInModules.ml
