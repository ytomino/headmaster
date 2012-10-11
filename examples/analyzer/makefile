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

# export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

TEST=test_analyzer

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(TEST)$(EXEEXT) test interactive clean

$(TEST)$(EXEEXT): $(TEST).ml
ifneq ($(OCAMLMAKE),)
	$(OCAMLMAKE) -g -w Aer -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $<
else
	-ln -s $(SOURCEDIR) .
	ocamlbuild -X $(BUILDDIR) -I source $(TEST).byte
	ln -sf $(TEST).byte $(TEST)$(EXEEXT)
endif

test: $(TEST)$(EXEEXT)
	./$(TEST)$(EXEEXT) --gcc "$(GCC)" $(HFLAGS) $(H)

interactive:
ifneq ($(OCAMLMAKE),)
	$(RLWRAP) $(OCAMLMAKE) -interact -g -D $(BUILDDIR) -L $(BUILDDIR) -I$(SOURCEDIR) $(TEST).ml
endif

clean:
	-$(RMDIR) build
	-$(RM) $(TEST)$(EXEEXT)
ifeq ($(OCAMLMAKE),)
	-$(RM) source
	ocamlbuild -clean
endif
