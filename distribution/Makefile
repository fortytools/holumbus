# Making all
#
# Building the library and generating documentation is done by cabal,
# only the tests and the examples will be built using make.

# TEST_BASE	= test
EXAMPLES_BASE	= Examples
PROGRAMS_BASE	= Programs

all : configure build doc dist

configure :
	@runhaskell Setup.hs configure

doc	: configure
	@runhaskell Setup.hs haddock --hyperlink-source --hscolour-css=hscolour.css

build	: configure
	@runhaskell Setup.hs build

install :
	@runhaskell Setup.hs install --global

dist	:
	@runhaskell Setup.hs sdist

prof	:
	@runhaskell Setup.hs configure -p
	@runhaskell Setup.hs build

# alltests :
#	$(MAKE) -C $(TEST_BASE) all

examples :
	$(MAKE) -C $(EXAMPLES_BASE) all

programs :	
	$(MAKE) -C $(PROGRAMS_BASE) all

wc :
	@wc -l `find ./source/Holumbus -wholename './_darcs/*' -prune -o -name "*.hs" -print`   
	# $(MAKE) -C $(TEST_BASE) wc
	$(MAKE) -C $(EXAMPLES_BASE) wc
	$(MAKE) -C $(PROGRAMS_BASE) wc

zip:	clean
	@zip -r holumbus_mapreduce$$(date +%Y%m%d_%H%M).zip *

clean :
	@runhaskell Setup.hs clean
	@rm -rf holumbus_mapreduce*.zip
	# $(MAKE) -C $(TEST_BASE) clean
	$(MAKE) -C $(EXAMPLES_BASE) clean
	$(MAKE) -C $(PROGRAMS_BASE) clean

.PHONY	: configure build doc dist clean zip wc examples programs