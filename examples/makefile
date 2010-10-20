DIRS=$(patsubst %/makefile,%,$(wildcard */makefile))
BDIRS=$(addsuffix .build,$(DIRS))
CDIRS=$(addsuffix .clean,$(DIRS))

.PHONY: all clean

all:
	make $(BDIRS)

clean:
	make $(CDIRS)
	make -C ../lib/gmp-ocaml clean
	make -C ../lib/unicode-ocaml clean

%.build: %
	make -C $(basename $@)

%.clean: %
	make -C $(basename $@) clean
