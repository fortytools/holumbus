HC	= ghc -i../../src -Wall -O2


all	: CrawlerSi

run	:
	./CrawlerSi contents.html

clean	:
	rm -f *.hi *.o

distclean	: clean
	rm CrawlerSi

CrawlerSi	: CrawlerSi.hs TextDocs.hs
		$(HC) -O2 --make -o $@ $<


