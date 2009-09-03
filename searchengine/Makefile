# Making all
#
# Building the library and generating documentation is done by cabal,
# only the tests and the examples will be built using make.

TEST_BASE	= test
EXAMPLES_BASE	= examples

all : configure build doc

configure :
	@runhaskell Setup.hs configure

doc	: configure
	@runhaskell Setup.hs haddock --hyperlink-source --hscolour-css=hscolour.css

build	: configure
	@runhaskell Setup.hs build

install :
	@sudo runhaskell Setup.hs install --global

prof	:
	@runhaskell Setup.hs configure -p
	@runhaskell Setup.hs build

alltests :
	$(MAKE) -C $(TEST_BASE) all

allexamples :
	$(MAKE) -C $(EXAMPLES_BASE) all

wc	:
	@wc -l `find ./source -wholename './_darcs/*' -prune -o -name "*.hs" -print`   
	$(MAKE) -C $(TEST_BASE) wc
	$(MAKE) -C $(EXAMPLES_BASE) wc

package dist	: configure
	@runhaskell Setup.hs sdist

clean :
	@runhaskell Setup.hs clean
	$(MAKE) -C $(TEST_BASE) clean
	$(MAKE) -C $(EXAMPLES_BASE) clean

.PHONY	: all configure doc build install prof alltests allexamples wc dist clean package
