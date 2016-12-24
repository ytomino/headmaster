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

# export CAML_LD_LIBRARY_PATH=$(abspath $(BUILDDIR)/stublibs)
export OCAMLRUNPARAM=b

TEST=test_analyzer
EXENAME=$(BUILDDIR)/$(TEST)$(EXEEXT)

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(EXENAME) all test interactive clean

all: $(EXENAME) $(BINLN)

$(EXENAME): $(TEST).ml
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -o $@ -g -safe-string -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -build-dir $(BUILDDIR) -X bin -I source $(TEST).byte
	cd $(BUILDDIR) && ln -sf $(TEST).byte $(notdir $(EXENAME))
endif

$(BINLN):
	ln -sfh $(BUILDDIR) $@

test: $(EXENAME)
	$(EXENAME) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -g -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(TEST).ml
endif

clean:
	-$(RMDIR) $(BUILDDIR)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
endif
	-$(RM) bin
