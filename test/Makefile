DKCHECK=dkcheck
DKDEP=dkdep

DIR=Dedukti

FILES=$(shell $(DKDEP) -s $(DIR)/*)

.PHONY: default
default: dedukti

.PHONY: dedukti
dedukti:
	$(DKCHECK) -e -I Dedukti $(FILES)
