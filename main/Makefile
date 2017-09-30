HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(ComSpec),)
EXEEXT=.exe
RM=del
RMDIR=rmdir /s /q
else
EXEEXT=
RM=rm
RMDIR=rm -rf
endif

BUILDDIR=$(TARGET).build
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

ifneq ($(wildcard ../.git),)
HEAD=$(wildcard ../.git/*HEAD) $(wildcard ../.git/refs/heads/master)
else
HEAD=
endif

OCAMLCFLAGS=-g -safe-string -w Aer-48

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
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

version.ml: version.txt $(HEAD)
	echo "let version = \"version $(shell ./version)\";;" > version.ml

$(BUILDDIR)/gmp.cmxa:
	make -C ../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	make -C ../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

$(BINLN):
	ln -sfh $(BUILDDIR) $@

$(SOURCELN):
	ln -sfh $(SOURCEDIR) $@

man: man1/headmaster.1

man1:
	mkdir man1

man1/headmaster.1: headmaster.1.rst version.rst | man1
	rst2man.py $< $@

version.rst: version.txt $(HEAD)
	echo ".. |version| replace:: $(shell ./version)" > version.rst

testman: man1/headmaster.1
	man -M . headmaster

clean:
	-$(RMDIR) $(BUILDDIR)
	-$(RMDIR) man1
	-$(RM) version.ml version.rst $(BINLN) $(SOURCELN)
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
