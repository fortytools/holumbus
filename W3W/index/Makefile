OPTS	=

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
ixn	= new-$(ix)

bindir  = $(HOME)/.cabal/bin

indexer	= $(bindir)/w3wIndexer
search	= $(bindir)/w3wSearch

progs	= $(indexer) # $(search)

# the -A option is important for garbage collection performance, a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

N       = 1
H       = 500
A       = 8
K       = 100
RUNOPTS =+RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -RTS

all	:
	cd .. && cabal configure && cabal build && cabal install

$(progs)	:
	cd .. && cabal configure && cabal build && cabal install

force	:
	rm -f $(progs)
	cd .. && cabal clean
	$(MAKE) all

whole-cache	: $(indexer)
	@echo "load w3w cache from fh-wedel.de and ptl.de, all pages are (re-)loaded, parsed and stored in binary form in subdir cache"
	@echo 'the list of loaded pages is written into file "cache.xml"'
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< $(RUNOPTS) --build-cache --maxthreads=$(threads) --valid=$(valid) --xml-output=cache.xml
# --test-index

whole-index	: $(indexer)
	@echo "build an index of all haddock pages on hackage in a single run"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo "the computation is done in 1 single run, but the index is spit into chunks"
	@echo " and written out chunk by chunk, then at the end these chunks are combined into the complete index"
	@echo "preparing and writing the compressed index requires most of the runtime"
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --partition=1000 --maxpar=1000 --maxdocs=30000 --valid=$(validix) --new-index=$(ix) --new-search=$(ix) $(OPTS)


whole-index1	: $(indexer)
	@echo "build an index of all fhw and ptl pages"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo 'the result as xml is "$(ix).xml"'
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=01000 --valid=$(validix) --new-index=$(ix)-01000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=02000 --valid=$(validix) --new-index=$(ix)-02000 --resume=tmp/ix-0000001000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=03000 --valid=$(validix) --new-index=$(ix)-03000 --resume=tmp/ix-0000002000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=04000 --valid=$(validix) --new-index=$(ix)-04000 --resume=tmp/ix-0000003000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=05000 --valid=$(validix) --new-index=$(ix)-05000 --resume=tmp/ix-0000004000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=06000 --valid=$(validix) --new-index=$(ix)-06000 --resume=tmp/ix-0000005000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=07000 --valid=$(validix) --new-index=$(ix)-07000 --resume=tmp/ix-0000006000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=08000 --valid=$(validix) --new-index=$(ix)-08000 --resume=tmp/ix-0000007000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=09000 --valid=$(validix) --new-index=$(ix)-09000 --resume=tmp/ix-0000008000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=10000 --valid=$(validix) --new-index=$(ix)-10000 --resume=tmp/ix-0000009000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=11000 --valid=$(validix) --new-index=$(ix)-11000 --resume=tmp/ix-0000010000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=1000 --maxdocs=12000 --valid=$(validix) --new-index=$(ix)-12000 --resume=tmp/ix-0000011000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=12500 --valid=$(validix) --new-index=$(ix)-12500 --resume=tmp/ix-0000012000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=13000 --valid=$(validix) --new-index=$(ix)-13000 --resume=tmp/ix-0000012500 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=13500 --valid=$(validix) --new-index=$(ix)-13500 --resume=tmp/ix-0000013000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=14000 --valid=$(validix) --new-index=$(ix)-14000 --resume=tmp/ix-0000013500 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=14500 --valid=$(validix) --new-index=$(ix)-14500 --resume=tmp/ix-0000014000 $(OPTS)
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=0500 --maxdocs=15000 --valid=$(validix) --new-index=$(ix)       --resume=tmp/ix-0000014500 --new-search=$(ix) $(OPTS) 

# --test-index
# --xml-output=$(ix).xml

whole	:
	@echo "update cache for all pages, this may run about 1 to 2 hours"
	$(MAKE) whole-cache 2> whole-cache.out
	@echo "create index for all pages"
	$(MAKE) whole-index 2> whole-ix.out
	@echo "look at the following files"
	@ls -l *.bin *.out

new2cur	: $(ixn).doc $(ixn).idx $(pxn).doc $(pxn).idx
	mv -f $(ix) $(ix)~
	mv    $(ixn)     $(ix)
	mv    $(ixn).doc $(ix).doc
	mv    $(ixn).idx $(ix).idx
	rm -f tmp/ix*

ixdir	= ./lib

install-index	: $(ix).doc $(ix).idx
	[ -d $(ixdir) ] || mkdir $(ixdir)
	cp -v $(ix).doc $(ix).idx

clean	:
	rm -f *.o *.hi $(progs) *.out

reset	:
	rm -rf cache/* tmp/*
