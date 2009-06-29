HC	= ghc -i../../src -Wall -O2

PROG	= ./sprueche
TARGET	= FussballerSprueche.hs

all	: $(PROG)

run	:
	$(MAKE) tmp
	$(PROG) $(TARGET)
	$(HC) -c $(TARGET)

clean	:
	rm -f *.hi *.o
	rm -rf tmp

distclean	: clean
	rm $(PROG)


$(PROG)	: Crawl.hs
	$(HC) -O2 --make -o $@ $<

tmp	:
	[ -d "$@" ] || mkdir "$@"

