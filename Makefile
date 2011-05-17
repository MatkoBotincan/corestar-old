# section that changes often

ifndef CORESTAR_HOME
	CORESTAR_HOME=$(CURDIR)
endif
export CORESTAR_HOME

SRC_DIRS=abs_int parsing plugin_interface prover prover_syntax proverfront \
				 symbexe symbexe_syntax symbfront utils
MAINS=symbfront test_symb test_logic
LIBS=dynlink str unix


# section that shouldn't change often

OCAMLBUILD=ocamlbuild $(addprefix -I src/,$(SRC_DIRS)) $(addprefix -lib ,$(LIBS))

build: native

native byte:
	$(OCAMLBUILD) $(addsuffix .$@,$(MAINS))
	for f in $(MAINS); do ln -sf ../`readlink $$f.$@` bin/$$f; rm $$f.$@; done

test: build
	$(MAKE) -s -C unit_tests

doc:
	$(MAKE) -C doc/tutorial

scripts:
	$(MAKE) -C scripts

all: build test

clean:
	ocamlbuild -clean
	rm -f lib/*.a lib/*.cmxa lib/*.cmxs bin/*
	$(MAKE) -C unit_tests clean
	$(MAKE) -C scripts clean
	$(MAKE) -C doc/tutorial clean

.PHONY: all build byte clean doc native scripts test

-include .install.mk

#vim:noet:
