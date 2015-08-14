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

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

EXENAME=$(BUILDDIR)/headmaster$(EXEEXT)

.PHONY: $(EXENAME) all man testman clean install

all: $(EXENAME) man $(BINLN)

$(EXENAME): main.ml version.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -g -safe-string -w Aer-48 -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $<
else
	-ln -s ../source .
	ocamlbuild -build-dir $(BUILDDIR) -X bin -I source -I source/ada main.native
	cd $(BUILDDIR) && ln -sf main.native $(notdir $(EXENAME))
endif

version.ml: version.txt $(HEAD)
	echo "let version = \"version $(shell ./version)\";;" > version.ml

$(BUILDDIR)/gmp.cmxa:
	make -C ../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	make -C ../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

$(BINLN):
	ln -s $(BUILDDIR) $@

man: man1/headmaster.1

man1/headmaster.1: headmaster.1.rst version.rst
	-mkdir man1
	rst2man.py $< $@

version.rst: version.txt $(HEAD)
	echo ".. |version| replace:: $(shell ./version)" > version.rst

testman: man1/headmaster.1
	man -M . headmaster

clean:
	-$(RMDIR) $(BUILDDIR)
	-$(RMDIR) man1
ifeq ($(OCAMLMAKE),)
	-$(RM) source
endif
	-$(RM) version.ml version.rst bin
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
