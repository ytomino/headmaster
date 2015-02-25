ifneq ($(ComSpec),)
EXEEXT=.exe
RM=del
RMDIR=rmdir /s /q
else
EXEEXT=
RM=rm
RMDIR=rm -rf
endif

BUILDDIR=build
SOURCEDIR=../source
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

EXENAME=headmaster$(EXEEXT)

.PHONY: $(EXENAME) man testman clean install

$(EXENAME): main.ml version.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -g -safe-string -w Aer-48 -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $<
else
	-ln -s ../source .
	ocamlbuild -X $(BUILDDIR) -I source -I source/ada main.native
	ln -sf main.native $(EXENAME)
endif

version.ml: version.txt $(HEAD)
	echo "let version = \"version $(shell ./version)\";;" > version.ml

$(BUILDDIR)/gmp.cmxa:
	make -C ../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	make -C ../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

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
	-$(RM) $(EXENAME)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
