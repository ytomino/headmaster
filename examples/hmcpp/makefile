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
SOURCEDIR=../../source
ifneq ($(HOMELOCAL),)
PREFIX=$(HOMELOCAL)
else
PREFIX=.
endif
BINDIR=$(PREFIX)/bin

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

EXENAME=hmcpp$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) test clean

$(EXENAME): hmcpp.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -O -g -safe-string -w Aer-48 -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -X $(BUILDDIR) -I source hmcpp.native
	ln -sf hmcpp.native $(EXENAME)
endif

test: $(EXENAME)
	./$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

$(BUILDDIR)/gmp.cmxa:
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr OCAMLCFLAGS=-safe-string

$(BUILDDIR)/unicode.cmxa:
	make -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR)) OCAMLCFLAGS=-safe-string

clean:
	-$(RMDIR) $(BUILDDIR)
	-$(RM) $(EXENAME)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
