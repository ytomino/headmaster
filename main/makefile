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
DESTDIR=$(HOMELOCAL)
else
DESTDIR=.
endif
BINDIR=$(DESTDIR)/bin

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

EXENAME=headmaster$(EXEEXT)

.PHONY: $(EXENAME) man testman clean install

$(EXENAME): main.ml $(BUILDDIR)/gmp.cmxa $(BUILDDIR)/unicode.cmxa
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -O -g -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) -I$(SOURCEDIR)/ada $<
else
	-ln -s ../source .
	ocamlbuild -X $(BUILDDIR) -I source -I source/ada main.native
	ln -sf main.native $(EXENAME)
endif

$(BUILDDIR)/gmp.cmxa:
	make -C ../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR)) WITH=gmp,mpfr

$(BUILDDIR)/unicode.cmxa:
	make -C ../lib/unicode-ocaml install DESTDIR=$(abspath $(BUILDDIR))

man: man1/headmaster.1

man1/headmaster.1: headmaster.1.rst
	-mkdir man1
	rst2man.py $< $@

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
