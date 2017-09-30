DIRS=$(patsubst %/Makefile,%,$(wildcard */Makefile))
BDIRS=$(addsuffix .build,$(DIRS))
CDIRS=$(addsuffix .clean,$(DIRS))
TDIRS=$(addsuffix .tests,$(DIRS))

.PHONY: all clean

all: $(BDIRS)

clean: $(CDIRS)

%.build: %
	$(MAKE) -C $<

%.clean: %
	$(MAKE) -C $< clean

%.tests: %
	make -C $< test
	make -C $< test H=../cc.h
	make -C $< test H=../posix.h
	make -C $< test GCC='gcc -m32 -isysroot /Developer/SDKs/MacOSX10.5.sdk -mmacosx-version-min=10.5'
	make -C $< test GCC='gcc -isysroot /Developer/SDKs/MacOSX10.6.sdk -mmacosx-version-min=10.6'
	make -C $< test GCC='gcc -isysroot /Developer/SDKs/MacOSX10.7.sdk -mmacosx-version-min=10.7'
	make -C $< test GCC='gcc -isysroot /Developer/SDKs/MacOSX10.8.sdk -mmacosx-version-min=10.8'
	make -C $< test GCC='gcc -isysroot /Developer/SDKs/MacOSX10.9.sdk -mmacosx-version-min=10.9'
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
	make -C $< test GCC='gcc -std=gnu99' H=../use-gmp.h
	make -C $< test GCC='i686-pc-freebsd7-gcc -std=gnu99' H=../use-gmp.h
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
