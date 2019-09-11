.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

xmlcoqdk:
	@ln -s _build/install/default/bin/xmlcoqdk xmlcoqdk || true

.PHONY: debug
debug:

	echo $(DEP)

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
