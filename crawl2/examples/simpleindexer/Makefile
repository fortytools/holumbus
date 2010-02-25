prog	= ./indexer

GHCOPTS	= -O2 -threaded -i../../src -ignore-package crawl2

all	: $(prog)

$(prog)	: SimpleIndexer.hs
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(prog)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"

run	: $(prog)
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$(prog) +RTS -N1 -s -K100M -RTS "" tmp/ix

clean	:
	rm -f *.o *.hi $(prog)

reset	:
	rm -rf cache/* tmp/*
