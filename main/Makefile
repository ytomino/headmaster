HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

RST2MAN=$(firstword $(wildcard /usr/bin/rst2man) rst2man.py)

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
GMP_BUILDDIR=$(BUILDDIR)/gmp$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

SOURCEDIR=../source
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
MANDIR=$(PREFIX)/share/man

ifneq ($(wildcard ../.git),)
HEAD=$(wildcard ../.git/*HEAD) $(wildcard ../.git/refs/heads/master)
else
HEAD=
endif

OCAMLCFLAGS=-g -w Aer-48

export OCAMLRUNPARAM=b

EXENAME=$(BUILDDIR)/headmaster$(EXEEXT)

.PHONY: $(EXENAME) all man testman clean install

all: $(EXENAME) man $(BINLN)

$(EXENAME): main.ml version.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -I $(SOURCELN)/ada -cflags "$(OCAMLCFLAGS)" main.native
	cd $(BUILDDIR) && ln -sf main.native $(notdir $(EXENAME))
endif

version.ml: version.txt version $(HEAD)
	echo "let version = \"version $(shell ./version)\";;" > version.ml

$(BUILDDIR)/gmp.cmxa:
	$(MAKE) -C ../lib/gmp-ocaml install \
		BUILDDIR=$(abspath $(GMP_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		WITH=gmp,mpfr

$(BUILDDIR)/unicode.cmxa:
	$(MAKE) -C ../lib/unicode-ocaml install \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	ln -s $(BUILDDIR) $@

$(SOURCELN):
	ln -s $(SOURCEDIR) $@

man: man1/headmaster.1

man1:
	mkdir man1

man1/headmaster.1: headmaster.1.rst version.rst | man1
	$(RST2MAN) $< $@

version.rst: version.txt version $(HEAD)
	echo ".. |version| replace:: $(shell ./version)" > version.rst

testman: man1/headmaster.1
	man -M . headmaster

clean:
	-rm -r $(BUILDDIR) man1
	-rm version.ml version.rst $(BINLN) $(SOURCELN)

install-bin: $(EXENAME) | $(BINDIR)
	install $(EXENAME) $(BINDIR)/$(notdir $(EXENAME))

install: install-bin man1/headmaster.1 | $(MANDIR)/man1
	install man1/headmaster.1 $(MANDIR)/man1/headmaster.1

$(BINDIR):
	mkdir -p $@

$(MANDIR)/man1:
	mkdir -p $@
