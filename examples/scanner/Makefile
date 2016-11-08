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
ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif
endif

BUILDDIR=$(TARGET).build
SOURCEDIR=../../source
BINLN=bin

export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

TEST=test_scanner
EXENAME=$(BUILDDIR)/$(TEST)$(EXEEXT)

.PHONY: $(EXENAME) all test interactive clean

all: $(EXENAME) | $(BINLN)

$(EXENAME): $(TEST).ml $(BUILDDIR)/gmp.cma $(BUILDDIR)/unicode.cma
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -g -safe-string -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -build-dir $(BUILDDIR) -X bin -I source $(TEST).byte
	cd $(BUILDDIR) && ln -sf $(TEST).byte $(notdir $(EXENAME))
endif

$(BINLN):
	ln -s $(BUILDDIR) $@

test: $(EXENAME)
	$(EXENAME)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -g -w LZ -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(TEST).ml
endif

$(BUILDDIR)/gmp.cma:
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cma:
	make -C ../../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

clean:
	-$(RMDIR) $(BUILDDIR)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
endif
	-$(RM) bin