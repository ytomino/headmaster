HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

RLWRAP?=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
GMP_BUILDDIR=$(BUILDDIR)/gmp$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

SOURCEDIR=../../source
BINLN=bin
ifneq ($(OCAMLMAKE),)
 SOURCELN=
else
 SOURCELN=source
endif

OCAMLCFLAGS=-g -w A-4-9-48

export OCAMLRUNPARAM=b

TEST=test_scanner
LIBNAME=$(BUILDDIR)/$(TEST).cma

.PHONY: $(LIBNAME) all test interactive clean

all: $(LIBNAME) $(BINLN)

$(LIBNAME): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -a -o $@ -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -cflags "$(OCAMLCFLAGS)" $(TEST).cma
endif

$(BINLN):
	ln -s $(BUILDDIR) $@

$(SOURCELN):
	ln -s $(SOURCEDIR) $@

test: $(LIBNAME)
	ocaml -I $(BUILDDIR) bigarray.cma unix.cma gmp.cma unicode.cma $(LIBNAME) testcase.ml --ocaml64
	ocaml -I $(BUILDDIR) bigarray.cma unix.cma gmp.cma unicode.cma $(LIBNAME) testcase.ml --gmp

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
else
	ocaml -I $(BUILDDIR) bigarray.cma unix.cma gmp.cma unicode.cma $(LIBNAME)
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

clean:
	-rm -r $(BUILDDIR)
	-rm $(BINLN) $(SOURCELN)
