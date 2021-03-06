HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
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

DESTDIR=
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

ifneq ($(wildcard ../../.git),)
HEAD=$(wildcard ../../.git/*HEAD) $(wildcard ../../.git/refs/heads/master)
else
HEAD=
endif

OCAMLCFLAGS=-g -w A-4-9-48

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
	$(MAKE) -C ../../lib/gmp-ocaml install \
		BUILDDIR=$(abspath $(GMP_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		WITH=gmp,mpfr

$(BUILDDIR)/unicode.cmxa:
	$(MAKE) -C ../../lib/unicode-ocaml install \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

clean:
	-rm -r $(BUILDDIR)
	-rm version.ml $(BINLN) $(SOURCELN)

install-bin: $(EXENAME) | $(DESTDIR)$(BINDIR)
	install $(EXENAME) $(DESTDIR)$(BINDIR)/$(notdir $(EXENAME))

install: install-bin

$(DESTDIR)$(BINDIR):
	mkdir -p $@
