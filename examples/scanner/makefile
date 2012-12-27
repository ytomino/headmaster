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

TEST=test_scanner

.PHONY: $(TEST)$(EXEEXT) test interactive clean

$(TEST)$(EXEEXT): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -g -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -X $(BUILDDIR) -I source $(TEST).byte
	ln -sf $(TEST).byte $(TEST)$(EXEEXT)
endif

test: $(TEST)$(EXEEXT)
	./$(TEST)$(EXEEXT)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -g -w LZ -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	make -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

clean:
	-$(RMDIR) build
	-$(RM) $(TEST)$(EXEEXT)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif
