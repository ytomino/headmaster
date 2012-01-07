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
OCAMLMAKE=

SOURCEDIR=../../source

export CAML_LD_LIBRARY_PATH=$(abspath build/stublibs)
export OCAMLRUNPARAM=b

TEST=test_translator

GCC=gcc
H=../c-lib.h
HFLAGS=

.PHONY: $(TEST)$(EXEEXT) test interactive clean some_tests

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
	make -C ../../lib/gmp-ocaml install DESTDIR=$(abspath $(BUILDDIR))

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

some_tests:
	make test
	make test H=../cc.h
	make test H=../posix.h
	make test GCC=i686-pc-freebsd7-gcc
	make test GCC=i686-pc-freebsd7-gcc H=../cc.h
	make test GCC=i686-pc-freebsd7-gcc H=../posix.h
	make test GCC=i686-w64-mingw32-gcc
	make test GCC=i686-w64-mingw32-gcc H=../cc.h
	make test GCC=i686-w64-mingw32-gcc H=../mingw-ansi.h
	make test GCC=i686-w64-mingw32-gcc H=../mingw-wide.h
	make test H=../use-cairo.h
	make test H=../use-expat.h
	make test H=../use-gc.h
	make test H=../use-gc.h HFLAGS="--isystem /usr/local/i686-pc-freebsd7/local/include" GCC=i686-pc-freebsd7-gcc
	make test H=../use-gmp.h
	make test H=../use-iconv.h
	make test H=../use-openssl.h
	make test H=../use-png.h
	make test H=../use-readline.h
	make test H=../use-xml.h HFLAGS="--isystem /usr/include/libxml2"
	make test H=../use-xml.h HFLAGS="--isystem /usr/local/i686-pc-freebsd7/local/include/libxml2" GCC=i686-pc-freebsd7-gcc
	make test H=../use-yaml.h
	make test H=../use-zlib.h
	make test H=testcase001.h
	make test H=testcase002.h
	make test H=testcase003.h
