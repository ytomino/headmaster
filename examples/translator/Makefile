HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
GMP_BUILDDIR=$(BUILDDIR)/gmp$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

SOURCEDIR=../../source
BINLN=bin
ifeq ($(OCAMLMAKE),)
SOURCELN=source
endif

OCAMLCFLAGS=-g -w A-4-9-48

export OCAMLRUNPARAM=b

TEST=test_translator
EXENAME=$(BUILDDIR)/$(TEST).byte$(EXEEXT)

IMPORTDIR=import.noindex

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

test: $(EXENAME) | $(IMPORTDIR)
	-rm $(IMPORTDIR)/*
	$(EXENAME) --destdir "$(IMPORTDIR)" --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	$(MAKE) -C ../../lib/gmp-ocaml install \
		BUILDDIR=$(abspath $(GMP_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	$(MAKE) -C ../../lib/unicode-ocaml install \
		BUILDDIR=$(abspath $(GMP_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(IMPORTDIR):
	mkdir $(IMPORTDIR)

clean:
	-rm -r $(BUILDDIR) $(IMPORTDIR)
	-rm $(BINLN) $(SOURCELN)
