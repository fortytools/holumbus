OPTS	=
DAYS	= 10

packages	= hxt,hxt-filter,hxt-binary,hxt-cache
latest	= 7days
valid	= 1hour
validix	= 1month
threads = 1

# threads = 1 means no parallel indexing but merging of partial indexes as a binary merge

# threads = 20 makes indexing much more efficent than a smaller figure,
# the merging of indexes is more efficient with a higher # of threads
# crawling may be a bit unfriendy to hackage it done with 20 threads

ix	= ix.bin
px	= pkg.bin
ixn	= new-$(ix)
pxn	= new-$(px)

src	= $(wildcard Hayoo/*.hs)

progs	= ./hayooIndexer search/hayooSearch

GHCOPTS	= -Wall -O2 -threaded -i.:../../src:../../../searchengine/source:../../../searchengine/examples/hayoo/source -ignore-package crawl2 -ignore-package Holumbus-Searchengine

N       = 1
H       = 3000
RUNOPTS = +RTS -N$(N) -s -K50M -H$(H)M -RTS

all	: $(progs)

./hayooIndexer	: HayooIndexer.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

search/hayooSearch	: HayooSearch.hs $(src)
	ghc  $(GHCOPTS) -o $@ --make $<

force	:
	rm -f $(progs)
	$(MAKE) GHCOPTS="$(GHCOPTS) -fforce-recomp"


whole-cache	: ./hayooIndexer
	@echo "load hayoo cache from hackage, all package and haddock pages are (re-)loaded, parsed and stored in binary form in subdir cache"
	@echo 'the list of loaded pages is written into file "cache.xml"'
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	./$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --valid=$(valid) --xml-output=cache.xml

whole-index	: ./hayooIndexer
	@echo "build an index of all haddock pages on hackage"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo "the computation is done in 3 steps to prevent use of too much main memory"
	@echo "preparing and writing the compressed index requires most of the runtime"
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=05000 --valid=$(validix) --new-index=$(ix)-05000 $(OPTS)
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=10000 --valid=$(validix) --new-index=$(ix)-10000 --resume=tmp/ix-0000005000 $(OPTS)
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=15000 --valid=$(validix) --new-index=$(ix)       --resume=tmp/ix-0000010000 --new-search=$(ix) $(OPTS)

whole-index1	: ./hayooIndexer
	@echo "build an index of all haddock pages on hackage in a single run"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo "the computation is done in 1 single run"
	@echo "preparing and writing the compressed index requires most of the runtime"
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=15000 --valid=$(validix) --new-index=$(ix) --new-search=$(ix) $(OPTS)

whole-pkg	: ./hayooIndexer
	@echo generate a new package index for all hackage packages inclusive ranking
	@echo the resulting package index is "$(px)", an XML version is stored in "$(px).xml" 
	./$< $(RUNOPTS) --pkg-index --ranking --maxthreads=$(threads) --maxpar=5000 --valid=$(validix) --new-index=$(px) --xml-output=$(px).xml --new-search=$(px) $(OPTS)

whole	:
	@echo "update cache for all hackage pages, this may run about 1 to 2 hours"
	$(MAKE) whole-cache 2> whole-cache.out
	@echo "create index for all haddock pages"
	$(MAKE) whole-index 2> whole-ix.out
	@echo "create index for hackage packages, this needs just a few minutes, output is $(px), xml output pkg.xml, log file is pkg.out"
	$(MAKE) whole-pkg 2> whole-pkg.out
	@echo look at the following files
	@ls -l *.bin *.out


update-cache	: ./hayooIndexer
	@echo update the hayoo cache with all packages uploaded to hackage within the last $(latest)
	@echo the list of loaded pages is written into file "cache.xml"
	./$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --maxpar=5000 --latest=$(latest) --xml-output=cache.xml

update-index	: ./hayooIndexer
	@echo update an existing hayoo index with all packages uploaded to hackage within the last $(latest)
	@echo existing index is "$(ix)", result index is "$(ixn)"
	./$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=15000 --latest=$(latest) --defragment --index=$(ix) --new-index=$(ixn)  --new-search=$(ixn)

update-pkg	: ./hayooIndexer
	@echo updating a package index is not neccessary, the complete index is ready within a minute
	$(MAKE) whole-pkg px=$(pxn)

update	:
	@echo "update hackage cache for latest packages"
	$(MAKE) update-cache
	@echo "update index with haddock doc pages of latest packages"
	$(MAKE) update-index
	@echo "create index for hackage packages"
	$(MAKE) update-pkg "OPT=$(OPT) --xml-output=$(px).xml"
	@echo look at the following files
	@ls -altr $(ixn)* $(pxn)*
	@echo if no faults occurred, exec "make new2cur" to make the new index the current one


load-cache	: ./hayooIndexer
	@echo load the following list of packages into local cache: $(packages)
	@echo use "make load-cache packages=pack1,pack2,pack3" to specify the packages 
	./$< +RTS -N$(N) -s -K100M -RTS --cache --valid=$(valid) --packages=$(packages) --xml-output=-

new2cur	: $(ixn).doc $(ixn).idx $(pxn).doc $(pxn).idx
	mv -f $(ix) $(ix)~
	mv -f $(px) $(px)~
	mv    $(ixn)     $(ix)
	mv    $(ixn).doc $(ix).doc
	mv    $(ixn).idx $(ix).idx
	rm -f tmp/ix*
	mv    $(pxn)     $(px)
	mv    $(pxn).doc $(px).doc
	mv    $(pxn).idx $(px).idx
	mv    $(pxn).xml $(px).xml
	rm -f tmp/pkg*

install-index	: $(ix).doc $(ix).idx $(px).doc $(px).idx
	cp -v $(ix).doc $(ix).idx $(px).doc $(px).idx search

clean	:
	rm -f *.o *.hi $(progs) *.out

reset	:
	rm -rf cache/* tmp/*
