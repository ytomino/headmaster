HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
 EXEEXT=.exe
else
 EXEEXT=
endif

RLWRAP?=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)

SOURCEDIR=../../source
BINLN=bin
ifeq ($(OCAMLMAKE),)
 SOURCELN=source
endif

OCAMLCFLAGS=-g -w A-4-9-48

export OCAMLRUNPARAM=b

TEST=test_analyzer
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
	ln -s $(BUILDDIR) $@

$(SOURCELN):
	ln -s $(SOURCEDIR) $@

test: $(EXENAME)
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(OCAMLCFLAGS) $(TEST).ml
endif

clean:
	-rm -r $(BUILDDIR)
	-rm $(BINLN) $(SOURCELN)
