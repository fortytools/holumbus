N       = 1

pkg	= hxt,hxt-filter,hxt-binary,hxt-cache

src	= $(wildcard Hayoo/*..hs)

progs	= ./hayooCache ./hayooIndexer


GHCOPTS	= -Wall -O2 -threaded -i../../src:../../../searchengine/source -ignore-package crawl2 -ignore-package Holumbus-Searchengine

all	: $(progs)

./hayooCache	: HayooCache.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

./hayooIndexer	: HayooIndexer.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(prog)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"

test	: cache

index	: ./hayooIndexer
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< +RTS -N$(N) -s -K100M -RTS -o tmp/ix.xml


cache	: ./hayooCache
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< +RTS -N$(N) -s -K100M -RTS -o tmp/ix.xml

update	: ./hayooCache
	$< +RTS -N$(N) -s -K100M -RTS -p "$(pkg)" -o tmp/ix.xml

recent	: ./hayooCache
	$< +RTS -N$(N) -s -K100M -RTS -r -o tmp/ix.xml


clean	:
	rm -f *.o *.hi $(progs)

reset	:
	rm -rf cache/* tmp/*
