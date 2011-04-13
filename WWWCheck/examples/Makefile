bindir	= $(HOME)/.cabal/bin

prog	= $(bindir)/WWWCheck

all	:
	cd .. && cabal configure && cabal build && cabal install

si	:
	$(prog) "http://localhost/~si/" check.localhost.si.html

java	:
	$(prog) "http://localhost/~si/vorlesungen/java/java.html" check.localhost.java.html

fhw.java	:
	$(prog) "http://www.fh-wedel.de/~si/vorlesungen/java/java.html" check.fh-wedel.java.html

fhw.fp	:
	$(prog) "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html" check.fh-wedel.fp.html

fhw.hxtdoc	:
	$(prog) "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html" check.fh-wedel.hxtdoc.html

fhw2	:
	$(prog) "http://www2.fh-wedel.de/" check.www2-fh-wedel.de.html

hobel.java	:
	$(prog) "http://192.168.2.11/~si/vorlesungen/java/java.html" check.hobel.java.html

holumbus	:
	$(prog) "http://holumbus.fh-wedel.de/" check.holumbus.html

clean	:
	rm -f check.*.html

distclean	: clean
	cd .. && cabal clean
	rm -f $(prog)

tmp	:
	[ -d "$@" ] || mkdir "$@"

