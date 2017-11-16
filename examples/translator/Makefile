HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif

BUILDDIR=$(TARGET).build
SOURCEDIR=../../source
BINLN=bin
ifeq ($(OCAMLMAKE),)
SOURCELN=source
endif

OCAMLCFLAGS=-g -safe-string -w Aer-48

export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

TEST=test_translator
EXENAME=$(BUILDDIR)/$(TEST).byte$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) all test interactive clean

all: $(EXENAME) $(BINLN)

$(EXENAME): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -I $(SOURCELN)/ada -cflags "$(OCAMLCFLAGS)" $(TEST).byte
endif

$(BINLN):
	ln -s $(BUILDDIR) $@

$(SOURCELN):
	ln -s $(SOURCEDIR) $@

test: $(EXENAME) | import
	-rm import/*
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	$(MAKE) -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	$(MAKE) -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

import:
	mkdir import

clean:
	-rm -r $(BUILDDIR) import
	-rm $(BINLN) $(SOURCELN)
