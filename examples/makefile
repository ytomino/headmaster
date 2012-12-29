DIRS=$(patsubst %/makefile,%,$(wildcard */makefile))
BDIRS=$(addsuffix .build,$(DIRS))
CDIRS=$(addsuffix .clean,$(DIRS))
TDIRS=$(addsuffix .tests,$(DIRS))

.PHONY: all clean

all:
	make $(BDIRS)

clean:
	make $(CDIRS)
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

%.build: %
	make -C $<

%.clean: %
	make -C $< clean

%.tests: %
	make -C $< test
	make -C $< test H=../cc.h
	make -C $< test H=../posix.h
	make -C $< test GCC='gcc -m32 -isysroot /Developer/SDKs/MacOSX10.5.sdk -mmacosx-version-min=10.5'
	make -C $< test GCC=i686-pc-freebsd7-gcc
	make -C $< test GCC=i686-pc-freebsd7-gcc H=../cc.h
	make -C $< test GCC=i686-pc-freebsd7-gcc H=../posix.h
	make -C $< test GCC=i686-w64-mingw32-gcc
	make -C $< test GCC=i686-w64-mingw32-gcc H=../cc.h
	make -C $< test GCC=i686-w64-mingw32-gcc H=../mingw-ansi.h
	make -C $< test GCC=i686-w64-mingw32-gcc H=../mingw-wide.h
	make -C $< test H=../use-cairo.h HFLAGS=`pkg-config --cflags-only-I cairo`
	make -C $< test H=../use-expat.h
	make -C $< test H=../use-expat.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-gc.h HFLAGS=`pkg-config --cflags-only-I bdw-gc`
	make -C $< test H=../use-gc.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-gmp.h
	make -C $< test H=../use-gmp.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-iconv.h
	make -C $< test H=../use-iconv.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-openssl.h
	make -C $< test H=../use-openssl.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-png.h
	make -C $< test H=../use-png.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-readline.h
	make -C $< test H=../use-readline.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-xml.h HFLAGS=`pkg-config --cflags-only-I libxml-2.0`
	make -C $< test H=../use-xml.h HFLAGS="-I/usr/local/i686-pc-freebsd7/include/libxml2" GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-yaml.h
	make -C $< test H=../use-yaml.h GCC=i686-pc-freebsd7-gcc
	make -C $< test H=../use-zlib.h
	make -C $< test H=../use-zlib.h GCC=i686-pc-freebsd7-gcc
