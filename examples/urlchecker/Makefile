HC	= ghc -i../../src -Wall -O2

PROG	= check-uris
SRC	= URIChecker.hs URIChecker/Check.hs URIChecker/Template.hs

all	: $(PROG) tmp

run	: si java

si	:
	./$(PROG) "http://localhost/~si/" check.localhost.si.html

java	:
	./$(PROG) "http://localhost/~si/vorlesungen/java/java.html" check.localhost.java.html

fhw.java	:
	./$(PROG) "http://www.fh-wedel.de/~si/vorlesungen/java/java.html" check.fh-wedel.java.html

fhw.fp	:
	./$(PROG) "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html" check.fh-wedel.fp.html

fhw.hxtdoc	:
	./$(PROG) "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html" check.fh-wedel.hxtdoc.html

fhw2	:
	./$(PROG) "http://www2.fh-wedel.de/" check.www2-fh-wedel.de.html

hobel.java	:
	./$(PROG) "http://192.168.2.11/~si/vorlesungen/java/java.html" check.hobel.java.html

holumbus	:
	./$(PROG) "http://holumbus.fh-wedel.de/" check.holumbus.html

clean	:
	rm -f *.hi *.o tmp/check-* check.*.html

distclean	: clean
	rm $(PROG)

$(PROG)	: $(SRC)
	$(HC) --make -o $@ $<

tmp	:
	[ -d "$@" ] || mkdir "$@"

