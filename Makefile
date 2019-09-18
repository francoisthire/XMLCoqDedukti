OPT=

.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build --profile release

.PHONY: doc
doc:
	@dune build @doc

xmlcoqdk:
	@ln -s _build/install/default/bin/xmlcoqdk xmlcoqdk || true

OUTPUT=test/Dedukti
XMLCOQDK = $(shell readlink -f _build/install/default/bin/xmlcoqdk)

$(OUTPUT):
	@mkdir $(OUTPUT)

.PHONY: test
test: bin $(OUTPUT)
	@$(XMLCOQDK) $(OPT) -o $(OUTPUT) `(cd test && find Coq -name "*.theory.xml")`
	make -C test dedukti

.PHONY: debug
debug:

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
