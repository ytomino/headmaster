HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)

SOURCEDIR=../../source
BINLN=bin
ifeq ($(OCAMLMAKE),)
SOURCELN=source
endif

ifneq ($(HOMELOCAL),)
PREFIX=$(HOMELOCAL)
else
PREFIX=.
endif
BINDIR=$(PREFIX)/bin

ifneq ($(wildcard ../../.git),)
HEAD=$(wildcard ../../.git/*HEAD) $(wildcard ../../.git/refs/heads/master)
else
HEAD=
endif

OCAMLCFLAGS=-g -safe-string -w Aer-48

export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

EXENAME=$(BUILDDIR)/hmcpp$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) all test clean

all: $(EXENAME) $(BINLN)

$(EXENAME): hmcpp.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa ../../main/version.ml version.ml $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -cflags "$(OCAMLCFLAGS)" hmcpp.native
	cd $(BUILDDIR) && ln -sf hmcpp.native $(notdir $(EXENAME))
endif

version.ml: | ../../main/version.ml
	ln -s $| $@

../../main/version.ml: ../../main/version.txt $(HEAD)
	$(MAKE) -C ../../main version.ml

$(BINLN):
	ln -s $(BUILDDIR) $@

$(SOURCELN):
	ln -s $(SOURCEDIR) $@

test: $(EXENAME)
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

$(BUILDDIR)/gmp.cmxa:
	$(MAKE) -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	$(MAKE) -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

clean:
	-rm -r $(BUILDDIR)
	-rm $(BINLN) $(SOURCELN)

install-bin: $(EXENAME) | $(BINDIR)
	install $(EXENAME) $(BINDIR)/$(notdir $(EXENAME))

install: install-bin

$(BINDIR):
	mkdir -p $@
