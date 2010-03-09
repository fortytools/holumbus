N       = 1

src	= HayooCache.hs HayooConfig.hs

prog	= ./updateCache

GHCOPTS	= -Wall -O2 -threaded -i../../src:../../../searchengine/source -ignore-package crawl2 -ignore-package Holumbus-Searchengine

all	: $(prog)

$(prog)	: $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(prog)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"

test	: run

run	: $(prog)
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$(prog) +RTS -N$(N) -s -K100M -RTS "" tmp/ix.xml

clean	:
	rm -f *.o *.hi $(prog)

reset	:
	rm -rf cache/* tmp/*
