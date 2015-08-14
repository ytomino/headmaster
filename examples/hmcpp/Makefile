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
SOURCEDIR=../../source
BINLN=bin

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

export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

EXENAME=$(BUILDDIR)/hmcpp$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) all test clean

all: $(EXENAME) $(BINLN)

$(EXENAME): hmcpp.ml version.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -g -safe-string -w Aer-48 -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -build-dir $(BUILDDIR) -X bin -I source hmcpp.native
	cd $(BUILDDIR) && ln -sf hmcpp.native $(notdir $(EXENAME))
endif

version.ml: ../../main/version.ml
	-ln -s $< $@

../../main/version.ml: ../../main/version.txt $(HEAD)
	make -C ../../main version.ml

$(BINLN):
	ln -s $(BUILDDIR) $@

test: $(EXENAME)
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

$(BUILDDIR)/gmp.cmxa:
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	make -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

clean:
	-$(RMDIR) $(BUILDDIR)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
endif
	-$(RM) bin

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
