# section that changes often

ifndef CORESTAR_HOME
	CORESTAR_HOME=$(CURDIR)
endif
export CORESTAR_HOME

SRC_DIRS=src
MAINS=corestar test_symb test_logic
LIBS=dynlink str unix

# section that shouldn't change often

SHELL=/bin/bash
SRC_SUBDIRS=$(addsuffix .subdirs,$(SRC_DIRS))
OCAMLBUILD=ocamlbuild `cat $(SRC_SUBDIRS)` $(addprefix -lib ,$(LIBS))

build: native

native byte: $(SRC_SUBDIRS)
	$(OCAMLBUILD) $(addsuffix .$@,$(MAINS))
	for f in $(MAINS); do ln -sf ../`readlink $$f.$@` bin/$$f; rm $$f.$@; done

test: test-native

test-native test-byte: test-%: %
	$(MAKE) -s -C unit_tests

doc:
	$(MAKE) -C doc/tutorial

scripts:
	$(MAKE) -C scripts

all: build test

clean:
	ocamlbuild -clean
	rm -f lib/*.a lib/* bin/* *.subdirs
	$(MAKE) -C unit_tests clean
	$(MAKE) -C scripts clean
	$(MAKE) -C doc/tutorial clean

%.subdirs: %
	ls -F $*/ | grep / | sed "s./.." | sed "s.^.-I $*/." > $*.subdirs

.PHONY: all build byte clean doc native scripts test

-include .install.mk

#vim:noet:
