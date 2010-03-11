N       = 1

pkg	= hxt,hxt-filter,hxt-binary,hxt-cache

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
	$(prog) +RTS -N$(N) -s -K100M -RTS -o tmp/ix.xml

update	: $(prog)
	$(prog) +RTS -N$(N) -s -K100M -RTS -p "$(pkg)" -o tmp/ix.xml

recent	: $(prog)
	$(prog) +RTS -N$(N) -s -K100M -RTS -r -o tmp/ix.xml


clean	:
	rm -f *.o *.hi $(prog)

reset	:
	rm -rf cache/* tmp/*
