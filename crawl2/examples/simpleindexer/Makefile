prog	= ./indexer

all	: $(prog)

$(prog)	: SimpleIndexer.hs
	ghc -O2 -threaded -o $@ -i../../src -ignore-package crawl2 --make $<

clean	:
	rm -f *.o *.hi $(prog)
