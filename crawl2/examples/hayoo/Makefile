OPT	=
DAYS	= 10

packages	= hxt,hxt-filter,hxt-binary,hxt-cache
latest	= 7days
valid	= 1hour
threads = 0

ix	= ix.bin
px	= pkg.bin

src	= $(wildcard Hayoo/*.hs)

progs	= ./hayooIndexer

GHCOPTS	= -Wall -O2 -threaded -i../../src:../../../searchengine/source -ignore-package crawl2 -ignore-package Holumbus-Searchengine

N       = 1
H       = 3000
RUNOPTS = +RTS -N$(N) -s -K50M -H$(H)M -RTS

all	: $(progs)

./hayooIndexer	: HayooIndexer.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(progs)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"


whole-cache	: ./hayooIndexer
	@echo load hayoo cache from hackage, all package and haddock pages are (re-)loaded, parsed and stored in binary form in subdir cache
	@echo the list of loaded pages is written into file "cache.xml"
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --valid=$(valid) --xml-output=cache.xml

whole-index	: ./hayooIndexer
	@echo build an index of all haddock pages on hackage
	@echo fill the local cache before running this, to avoid network traffic and to get an up to date index
	@echo the result index is "$(ix)"
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --new-index=$(ix)

whole-pkg	: ./hayooIndexer
	@echo generate a new package index for all hackage packages inclusive ranking
	@echo the resulting package index is "$(px)", an XML version is stored in "$(px).xml" 
	./$< $(RUNOPTS) --pkg-index --ranking --maxthreads=$(threads) --new-index=$(px) --xml-output=$(px).xml

whole	:
	@echo "update cache for all hackage pages, this may run about 1 to 2 hours"
	$(MAKE) whole-cache 2> cache.out
	@echo "create index for all haddock pages"
	$(MAKE) whole-index 2> ix.out
	@echo "create index for hackage packages, this needs just a few minutes, output is $(px), xml output pkg.xml, log file is pkg.out"
	$(MAKE) whole-pkg 2> pkg.out
	@echo look at the following files
	@ls -l *.bin *.out


update-cache	: ./hayooIndexer
	@echo update the hayoo cache with all packages uploaded to hackage within the last $(latest)
	@echo the list of loaded pages is written into file "cache.xml"
	./$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --latest=$(latest) --xml-output=cache.xml

update-index	: ./hayooIndexer
	@echo update an existing hayoo index with all packages uploaded to hackage within the last $(latest)
	@echo existing index is "$(ix)", result index is "$(ix).new"
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --latest=$(latest) --defragment --index=$(ix) --new-index=$(ix).new

update-pkg	: ./hayooIndexer
	@echo updating a package index is not neccessary, the complete index is ready within a minute
	$(MAKE) whole-pkg px=$(px).new

update	:
	@echo "update hackage cache for latest packages"
	$(MAKE) update-cache 2> cache.out
	@echo "update index with haddock doc pages of latest packages"
	$(MAKE) update-index 2> ix.out
	@echo "create index for hackage packages"
	$(MAKE) update-pkg "OPT=$(OPT) --xml-output=$(px).xml" 2> pkg.out
	@echo look at the following files
	@ls -l *.new *.out
	@echo if no faults occurred, exec "make new-to-cur" to make the new index the current one


load-cache	: ./hayooIndexer
	@echo load the following list of packages into local cache: $(packages)
	@echo use "make load-cache packages=pack1,pack2,pack3" to specify the packages 
	./$< +RTS -N$(N) -s -K100M -RTS --cache --valid=$(valid) --packages=$(packages) --xml-output=-

new-to-cur	: $(ix).new $(px).new
	mv -f $(ix) $(ix)~
	mv -f $(px) $(px)~
	mv    $(ix).new $(ix)
	mv    $(px).new $(px)
 
clean	:
	rm -f *.o *.hi $(progs) *.out

reset	:
	rm -rf cache/* tmp/*
