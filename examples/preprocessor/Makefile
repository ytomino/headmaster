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
ifeq ($(OCAMLMAKE),)
SOURCELN=source
endif

OCAMLCFLAGS=-g -safe-string -w Aer-48

# export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

TEST=test_preprocessor
EXENAME=$(BUILDDIR)/$(TEST).byte$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) all test interactive clean

all: $(EXENAME) $(BINLN)

$(EXENAME): $(TEST).ml $(SOURCELN)
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -D $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $<
else
	ocamlbuild -build-dir $(BUILDDIR) -X $(BUILDDIR) -X bin -I $(SOURCELN) -cflags "$(OCAMLCFLAGS)" $(TEST).byte
endif

$(BINLN):
	ln -sfh $(BUILDDIR) $@

$(SOURCELN):
	ln -sfh $(SOURCEDIR) $@

test: $(EXENAME)
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
endif

clean:
	-$(RMDIR) $(BUILDDIR)
	-$(RM) $(BINLN) $(SOURCELN)
