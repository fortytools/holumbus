RTOPTS			= +RTS -K100M -RTS

TESTS			= hayoo-persistent-to-memory \
                          hayoo-memory-to-prefixtree \
                          hayoo-count-occurrences \
                          hayoo-lookup-words

all			: ./HayooIndexTest $(TESTS)

run			: all
			  ./HayooIndexTest +RTS -K100M -RTS

run1			: all
			  hayoo-persistent-to-memory $(RTOPTS)

run2			: all
			  hayoo-memory-to-prefixtree $(RTOPTS)

run3			: all
			  hayoo-count-occurrences $(RTOPTS)

run4			: all
			  hayoo-lookup-words $(RTOPTS)

./HayooIndexTest  	: HayooIndexTest.hs ../../tmp/Occ.hs
			ghc -O2 -Wall -o $@ -i../../source:../../examples/hayoo/source:../../tmp --make $<

$(TESTS)		: ./HayooIndexTest
			  for i in $(TESTS) ; do ln ./HayooIndexTest $$i ; done

clean			:
			rm -f *.hi *.o $(TESTS) ./HayooIndexTest
