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
RUNOPTS = +RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -RTS

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
	$< $(RUNOPTS) --build-cache --maxthreads=$(threads) --valid=$(valid) --xml-output=cache.xml --test-index
# --test-index

whole-index	: $(indexer)
	@echo "build an index of all fhw and ptl pages"
	@echo "fill the local cache before running this, to avoid network traffic and to get an up to date index"
	@echo 'the result index is "$(ix)"'
	@echo 'the result as xml is "$(ix).xml"'
	$< $(RUNOPTS) --build-index --maxthreads=$(threads) --maxpar=50000 --maxdocs=50000 --valid=$(validix) --new-index=$(ix) --new-search=$(ix) $(OPTS) --test-index --xml-output=$(ix).xml
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
