ifneq ($(ComSpec),)
EXEEXT=.exe
RM=del
RMDIR=rmdir /s /q
else
EXEEXT=
RM=rm
RMDIR=rm -rf
ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif
endif

BUILDDIR=build
SOURCEDIR=../../source

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

TEST=test_translator

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(TEST)$(EXEEXT) test interactive clean

$(TEST)$(EXEEXT): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -g -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -X $(BUILDDIR) -I source -I source/ada $(TEST).byte
	ln -sf $(TEST).byte $(TEST)$(EXEEXT)
endif

test: $(TEST)$(EXEEXT) import
	-$(RM) import/*
	./$(TEST)$(EXEEXT) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -g -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	make -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

import:
	mkdir import

clean:
	-$(RMDIR) build
	-$(RMDIR) import
	-$(RM) $(TEST)$(EXEEXT)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif
