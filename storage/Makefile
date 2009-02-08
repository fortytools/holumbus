# Making all
#
# Building the library and generating documentation is done by cabal,
# only the tests and the examples will be built using make.

# TEST_BASE	= test
EXAMPLES_BASE	= Examples
#PROGRAMS_BASE   = Programs


all : configure build doc

configure :
	@runhaskell Setup.hs configure

doc	: configure
	@runhaskell Setup.hs haddock --hyperlink-source --hscolour-css=hscolour.css

build	: configure
	@runhaskell Setup.hs build

install :
	@runhaskell Setup.hs install --global

prof	:
	@runhaskell Setup.hs configure -p
	@runhaskell Setup.hs build

dist	:
	@runhaskell Setup.hs sdist

# alltests :
#	$(MAKE) -C $(TEST_BASE) all

examples :
	$(MAKE) -C $(EXAMPLES_BASE) all

#programs :
#	$(MAKE) -C $(PROGRAMS_BASE) all

wc :
	@wc -l `find ./source/Holumbus -wholename './_darcs/*' -prune -o -name "*.hs" -print`   
	# $(MAKE) -C $(TEST_BASE) wc
	$(MAKE) -C $(EXAMPLES_BASE) wc
	#$(MAKE) -C $(PROGRAMS_BASE) wc

zip:	clean
	@zip -r holumbus_storage$$(date +%Y%m%d_%H%M).zip *

clean :
	@runhaskell Setup.hs clean
	@rm -rf holumbus_storage*.zip
	# $(MAKE) -C $(TEST_BASE) clean
	$(MAKE) -C $(EXAMPLES_BASE) clean
	#$(MAKE) -C $(PROGRAMS_BASE) clean

.PHONY	: all configure doc build install prof dist examples wc zip clean

