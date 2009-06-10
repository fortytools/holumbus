HC	= ghc -i../../src -Wall -O2

PROG	: check-uris

all	: $(PROG) tmp

run	:
	./$(PROG) "http://localhost/~si/" check.si.html

clean	:
	rm -f *.hi *.o tmp/check-* check.si.html

distclean	: clean
	rm $(PROG)

$(PROG)	: URIChecker.hs URIChecker/Check.hs URIChecker/Template.hs resultTemplate.html
	$(HC) -O2 --make -o $@ $<

tmp	:
	[ -d "$@" ] || mkdir "$@"

