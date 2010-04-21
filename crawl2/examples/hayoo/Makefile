N       = 1

pkg	= hxt,hxt-filter,hxt-binary,hxt-cache

src	= $(wildcard Hayoo/*.hs)

progs	= ./hayooCache ./hayooIndexer

outch	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-cache.xml")
outix	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-ix.xml")
outbin	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-ix.xml")

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
	./$< +RTS -N$(N) -s -K100M -H1500M -RTS -i tmp/ix.bin -m 15000

cache	: ./hayooCache
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< +RTS -N$(N) -s -K100M -RTS -o $(outch)

update	: ./hayooCache
	./$< +RTS -N$(N) -s -K100M -RTS -p "$(pkg)" -o $(outch)

recent	: ./hayooCache
	./$< +RTS -N$(N) -s -K100M -RTS -u -o  $(outch)


clean	:
	rm -f *.o *.hi $(progs)

reset	:
	rm -rf cache/* tmp/*
