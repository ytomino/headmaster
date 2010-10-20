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
OCAMLMAKE=

SOURCEDIR=../source
ifneq ($(HOMELOCAL),)
DESTDIR=$(HOMELOCAL)
else
DESTDIR=.
endif
BINDIR=$(DESTDIR)/bin

export CAML_LD_LIBRARY_PATH=build/stublibs
export OCAMLRUNPARAM=b

EXENAME=headmaster$(EXEEXT)

.PHONY: $(EXENAME) clean install

$(EXENAME): main.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -g -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s ../source .
	ocamlbuild -X $(BUILDDIR) -I source main.native
	ln -sf main.native $(EXENAME)
endif

$(BUILDDIR)/gmp.cmxa:
	make -C ../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR))

$(BUILDDIR)/unicode.cmxa:
	make -C ../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

clean:
	-$(RMDIR) $(BUILDDIR)
	-$(RM) $(EXENAME)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

install: $(EXENAME)
	install $(EXENAME) $(BINDIR)
