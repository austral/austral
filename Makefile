BIN := austral
SRC := lib/*.ml lib/*.mli lib/*.mll lib/*.mly lib/dune bin/dune bin/austral.ml lib/BuiltInModules.ml

.PHONY: all
all: $(BIN)

lib/BuiltInModules.ml: lib/builtin/*.aui lib/builtin/*.aum lib/prelude.h lib/prelude.c
	python3 concat_builtins.py

$(BIN): $(SRC)
	opam exec -- dune build
	cp _build/default/bin/austral.exe $(BIN)

.PHONY: test
test: $(BIN)
	opam exec -- dune runtest

install: $(BIN)
	install -m 755 austral /usr/local/bin/austral

uninstall:
	sudo rm /usr/local/bin/austral

clean:
	rm $(BIN); rm -rf _build; rm lib/BuiltInModules.ml
