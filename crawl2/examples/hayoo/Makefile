N       = 1
OPT	=
DAYS	= 10

pkg	= hxt,hxt-filter,hxt-binary,hxt-cache

ix	= ix.bin
px	= pkg.bin

pxxml	=$(px:.bin=.xml)

src	= $(wildcard Hayoo/*.hs)

progs	= ./hayooCache ./hayooIndexer ./latestPackages

outch	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-cache.xml")
outix	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-ix.xml")
outbin	:= $(shell echo "tmp/$$(date +%Y-%m-%d-%H:%M:%S)-ix.xml")

GHCOPTS	= -Wall -O2 -threaded -i../../src:../../../searchengine/source -ignore-package crawl2 -ignore-package Holumbus-Searchengine

all	: $(progs)

./hayooCache	: HayooCache.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

./hayooIndexer	: HayooIndexer.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

./latestPackages	: LatestPackages.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(prog)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"

test	: cache

index	: ./hayooIndexer
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< +RTS -N$(N) -s -K100M -H1500M -RTS --index=$(ix) -m 15000 --fct-index $(OPT)

pkgix	: ./hayooIndexer
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< +RTS -N$(N) -s -K100M -H1500M -RTS --index=$(px) -m 15000 --pkg-index $(OPT)

cache	: ./hayooCache
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< +RTS -N$(N) -s -K100M -RTS -o $(outch) $(OPT)

update	: ./hayooCache
	./$< +RTS -N$(N) -s -K100M -RTS -p "$(pkg)" -o $(outch)

recent	: ./hayooIndexer
	@echo $(OPT)
	@echo not done: $(MAKE) archive
	[ -f $(ix).org ] || cp $(ix) $(ix).org
	./$< +RTS -N$(N) -s -K100M -RTS --update --packages=$$(./latestPackages $(DAYS)) --valid=1hour --index=$(ix) --maxdocs=15000 --fct-index

archive	:	
	wget http://hackage.haskell.org/packages/00-index.tar.gz -O tmp/00-index.tar.gz

latest	:
	./latestPackages $(DAYS)

complete	:
	@echo "get package archive from hackage"
	$(MAKE) archive
	@echo "get new packages from within the last 10 days"
	$(MAKE) latest
	@echo "update hackage cache, this may run about 1 to 2 hours, ./cache is the cache dir, log file is cache.out"
	$(MAKE) cache 2> cache.out
	@echo "create index for haddock doc pages, this may run about 10 to 20 minutes, output is $(ix), log file is ix.out"
	$(MAKE) index 2> ix.out
	@echo "create index for hackage packages, this needs just a few minutes, output is $(px), xml output pkg.xml, log file is pkg.out"
	$(MAKE) pkgix "OPT=$(OPT) --xml-output=$(pxxml)" 2> pkg.out
	@echo "make complete finished, look at $(ix), $(px) and $(pxxml), and the log files"

clean	:
	rm -f *.o *.hi $(progs)

reset	:
	rm -rf cache/* tmp/*
