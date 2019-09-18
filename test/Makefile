DKCHECK=dkcheck
DKDEP=dkdep

DIR=Dedukti

FILES=$(shell $(DKDEP) -I Dedukti -s $(DIR)/*.dk)

.PHONY: default
default: dedukti

.PHONY: dedukti
dedukti:
	$(DKCHECK) -e -I Dedukti $(FILES)

.PHONY: debug
debug:
	$(FILES)
