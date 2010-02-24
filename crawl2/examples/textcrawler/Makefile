HC	= ghc -i../../src -Wall -O2


all	: CrawlerSi tmp

run	:
	./CrawlerSi contents.html

clean	:
	rm -f *.hi *.o tmp/hc-*

distclean	: clean
	rm CrawlerSi

CrawlerSi	: CrawlerSi.hs TextDocs.hs
		$(HC) -O2 --make -o $@ $<

tmp	:
	[ -d "$@" ] || mkdir "$@"

