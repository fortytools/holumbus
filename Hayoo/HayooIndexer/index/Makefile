OPTS	=
DAYS	= 10

latest	= 7days
valid	= 1hour
validix	= 1month
threads = 1

# threads = 1 means no parallel indexing but merging of partial indexes as a binary merge

# threads = 20 makes indexing much more efficent than a smaller figure,
# the merging of indexes is more efficient with a higher # of threads
# crawling may be a bit unfriendy to hackage if done with 20 threads
# there is another problem with parallel crawling: curl is not yet thread save

ix	= ix.bin
px	= pkg.bin
ixn	= new-$(ix)
pxn	= new-$(px)

# bindir  = $(HOME)/.cabal/bin
bindir  = ../.cabal-sandbox/bin

indexer	= $(bindir)/hayooIndexer
snap	= $(bindir)/hayooSnap
# search	= $(bindir)/hayooSearch
# fastcgi	= $(bindir)/hayooFastCGI

progs	= $(indexer) $(snap)

# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

N       = 1
H       = 500
A       = 8
K       = 200
RUNOPTS = +RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -RTS

# partition: size of partial indexes build with whole-index
#            if size >= 1000, there occur "out of memory" errors when writing partial indexes
# maxpar:    maximum of docs indexed and merged in paralled ( <= partition )
# maxdocs:   upper limit of # of docs, hackage had about 33000 docs (2013-10-09)

partition =500
maxpar    =500
maxdocs   =50000

all	:
	cd .. && cabal configure && cabal build && cabal install

$(progs)	:
	cd .. && cabal configure && cabal build && cabal install

force	:
	rm -f $(progs)
	cd .. && cabal clean
	$(MAKE) all

whole-cache	: $(indexer)
	@echo "load hayoo cache from hackage, all package and haddock pages are (re-)loaded, parsed and stored in binary form in subdir cache"
	@echo 'the list of loaded pages is written into file "cache.xml"'
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< $(RUNOPTS) --cache \
		--hackage \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--valid=$(valid) \
		--xml-output=cache.xml \
                $(OPTS)
	ls -l cache.xml

whole-index-old	: $(indexer)
	@echo "build an index of all haddock pages on hackage"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo "the computation is done in 5 steps to prevent use of too much main memory, hackage has about 16900 pages"
	@echo "preparing and writing the compressed index requires most of the runtime"
	$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=05000 --valid=$(validix) --new-index=$(ix)-05000 $(OPTS)
	$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=5000 --maxdocs=10000 --valid=$(validix) --new-index=$(ix)-10000 --resume=tmp/ix-0000005000 $(OPTS)
	$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=2500 --maxdocs=12500 --valid=$(validix) --new-index=$(ix)-12500 --resume=tmp/ix-0000010000 $(OPTS)
	$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=2500 --maxdocs=15000 --valid=$(validix) --new-index=$(ix)-15000 --resume=tmp/ix-0000012500 $(OPTS)
	$< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=2500 --maxdocs=17500 --valid=$(validix) --new-index=$(ix)       --resume=tmp/ix-0000015000 --new-search=$(ix) $(OPTS)
	@echo not yet needed: $< $(RUNOPTS) --fct-index --maxthreads=$(threads) --maxpar=2500 --maxdocs=20000 --valid=$(validix) --new-index=$(ix)       --resume=tmp/ix-0000017500 --new-search=$(ix) $(OPTS)

whole-index	: $(indexer)
	@echo "build an index of all haddock pages on hackage in a single run"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo "the computation is done in 1 single run, but the index is spit into chunks"
	@echo " and written out chunk by chunk, then at the end these chunks are combined into the complete index"
	@echo "preparing and writing the compressed index requires most of the runtime"
	rm -f tmp/ix-*
	$< $(RUNOPTS) --fct-index \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--maxpar=$(maxpar) \
		--partition=$(partition) \
		--valid=$(validix) \
		--new-index=$(ix) \
		--new-search=$(ix) \
		$(OPTS)
	ls -l $(ix)*

whole	:
	[ -d ./log ]   || mkdir log
	@echo "update cache for all hackage pages, this may run about 1 to 2 hours"
	$(MAKE) whole-cache > log/whole-cache.out 2>&1
	@echo "create index for all haddock pages"
	$(MAKE) whole-index > log/whole-index.out 2>&1
	@echo "create index for hackage packages, this needs just a few minutes, output is $(px), xml output pkg.xml, log file is pkg.out"
	$(MAKE) whole-pkg   > log/whole-pkg.out   2>&1
	@echo look at the following files
	@ls -l *.bin* log/whole-*.out

#----------------------------------------

whole-json-index	: $(indexer)
	@echo "build an index of all haddock pages on hackage in a single run"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result is written in JSON format into subdir "functions"'
	@echo "there is a JSON file per hackage package"
	$< $(RUNOPTS) --fct-index \
		--json-output \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--maxpar=$(maxpar) \
		--valid=$(validix) \
		--new-index=$(ix) \
		--new-search=$(ix) \
		$(OPTS)
	ls ./functions/*.js

whole-json-pkg	: $(indexer)
	@echo generate a new package index for all hackage packages inclusive ranking
	@echo the resulting package index is "$(px)"
	@echo for an XML version, use 'make whole-pkg OPTS=--xml-output="$(px).xml"'
	rm -f tmp/pkg-*
	$< $(RUNOPTS) --pkg-index \
		--json-output \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--maxpar=$(maxdocs) \
		--valid=$(validix) \
		--new-index=$(px) \
		--new-search=$(px) \
		$(OPTS)
	ls -l ./json/00-packages.js

whole-json-rank	: $(indexer)
	@echo generate a new package index for all hackage packages inclusive ranking
	@echo the resulting package index is "$(px)"
	@echo for an XML version, use 'make whole-pkg OPTS=--xml-output="$(px).xml"'
	rm -f tmp/pkg-*
	$< $(RUNOPTS) --pkg-index \
		--json-output \
		--only-ranking \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--maxpar=$(maxdocs) \
		--valid=$(validix) \
		--new-index=$(px) \
		--new-search=$(px) \
		$(OPTS)
	ls -l ./json/00-ranking.js

whole-json	: $(indexer)
	$(MAKE) update-cache      > log/update-cache.out     2>&1
	$(MAKE) whole-json-index  > log/whole-json-index.out 2>&1
	$(MAKE) whole-json-pkg    > log/whole-json-pkg.out   2>&1
	$(MAKE) whole-json-rank   > log/whole-json-rank.out  2>&1
	ls -l ./json

#----------------------------------------

update-cache	: $(indexer)
	@echo update the hayoo cache with all packages uploaded to hackage within the last $(latest)
	@echo the list of loaded pages is written into file "cache.xml"
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --maxpar=$(maxpar) --latest=$(latest) --xml-output=cache.xml

update-index	: $(indexer)
	@echo update an existing hayoo index with all packages uploaded to hackage within the last $(latest)
	@echo with partial indexes the update becomes obsolete, the whole index is rebuild
	@echo same as "make whole-index" but result index is "$(ixn)"
	$(MAKE) whole-index ix=$(ixn)

update-pkg	: $(indexer)
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


packages	= hxt,hxt-unicode

load-cache	: $(indexer)
	@echo load the following list of packages into local cache: $(packages)
	@echo use "make load-cache packages=pack1,pack2,pack3" to specify the packages 
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< +RTS -N$(N) -s -K100M -RTS --cache --valid=$(valid) --packages=$(packages) --xml-output=-

new2cur	: $(ixn).doc $(ixn).idx $(pxn).doc $(pxn).idx
	mv -f $(ix) $(ix)~ || true
	mv -f $(px) $(px)~ || true
	mv    $(ixn)     $(ix) || true
	mv    $(ixn).doc $(ix).doc
	mv    $(ixn).idx $(ix).idx
	rm -f tmp/ix*
	mv    $(pxn)     $(px) || true
	mv    $(pxn).doc $(px).doc
	mv    $(pxn).idx $(px).idx
	mv    $(pxn).xml $(px).xml
	rm -f tmp/pkg*

ixdir	= ./lib

install-index	: $(ix).doc $(ix).idx $(px).doc $(px).idx
	[ -d $(ixdir) ] || mkdir $(ixdir)
	cp -v $(ix).doc $(ix).idx $(px).doc $(px).idx $(ixdir)

clean	:
	rm -f *.o *.hi $(progs) *.out

reset	:
	rm -rf cache/* tmp/*

pkglist = hxt-filter
ixx     = hxt-ix
pxx     = hxt-pkg

testix	:
	$(indexer) --fct-index --maxthreads=1 --maxpar=20 --maxdocs=2500 --valid=1month --new-index=$(ixx) --new-search=$(ixx) --xml-output=$(ixx).xml -p $(pkglist)

#	$(indexer) --fct-index --maxthreads=1 --maxpar=20 --maxdocs=2500 --valid=1month --new-index=$(ixx) --new-search=$(ixx) --xml-output=$(ixx).xml -p $(pkglist) --partition=20

testpkg	:
	$(indexer) --pkg-index --maxthreads=0 --maxpar=250 --maxdocs=2500 --valid=1month --new-index=$(pxx) --new-search=$(pxx) --xml-output=$(pxx).xml -p $(pkglist)

test	: testix testpkg

search	:
	$(snap) +RTS -s -K100M -RTS &

.PHONY	: all force \
	whole whole-cache whole-index whole-pkg \
	update update-cache update-index update-pkg \
	new2cur install-index \
	clean reset \
	test testpkg testix \
	search
