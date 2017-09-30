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

TEST=test_scanner
EXENAME=$(BUILDDIR)/$(TEST).byte$(EXEEXT)

.PHONY: $(EXENAME) all test interactive clean

all: $(EXENAME) $(BINLN)

$(EXENAME): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -cflags "$(OCAMLCFLAGS)" $(TEST).byte
endif

$(BINLN):
	ln -sfh $(BUILDDIR) $@

$(SOURCELN):
	ln -sfh $(SOURCEDIR) $@

test: $(EXENAME)
	$(EXENAME)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	$(MAKE) -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	$(MAKE) -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

clean:
	-rm -r $(BUILDDIR)
	-rm $(BINLN) $(SOURCELN)
