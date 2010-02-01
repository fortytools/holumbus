# Making all tests


# Make GHC compile everything seperatly for the tests, to be able to 
# execute them without installing the library first.
GHC_FLAGS	= -Wall -O2 -threaded -XRelaxedPolyRec -XBangPatterns -ignore-package Holumbus -i$(SOURCE) -odir $(OUTPUT) -hidir $(OUTPUT)
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

SOURCE		= ../source

TESTDATA  = data
OUTPUT    = output
COVERAGE  = coverage

TESTS     = BinaryTest CrunchTest DocumentsTest FuzzyTest InvertedTest \
            ParserTest PickleTest StrMapTest DiffListTest

PROG	= AllTests

all 	: cabal

test 	:
	$(MAKE) test1 test2

cabal	:
	cabal configure && cabal build

test1	:
	@echo "Running Holumbus function tests..."
	./dist/build/AllTests/AllTests  +RTS -K20M -RTS -u -q

test2	: space
	@echo "Running Holumbus space tests..."
	./dist/build/SpaceTest/SpaceTest

space	: bibel.txt

bibel.txt	:
	wget -O bibel.zip http://www.kahal.de/bibel_elb_bibel.zip
	unzip bibel.zip
	mv -f elb_bibel.txt bibel.txt
	rm -f bibel.zip

hpc :
	[ -d $(OUTPUT)_hpc ] || mkdir $(OUTPUT)_hpc
	$(GHC) -odir $(OUTPUT)_hpc -package QuickCheck-1.2.0.0 -hidir $(OUTPUT)_hpc -fhpc --make -o $(PROG)_hpc $(PROG).hs
	[ -d $(TESTDATA) ] || mkdir $(TESTDATA)
	@echo "Gathering coverage..."
	./$(PROG)_hpc  +RTS -K20M -RTS -u -q
	[ -d $(COVERAGE) ] || mkdir $(COVERAGE)
	@echo "Generating markup..."
	@hpc markup --destdir=$(COVERAGE) $(TESTS:%=--exclude=%) --exclude=Main --exclude=SampleData $(PROG)_hpc

clean :
	$(RM) $(PROG) $(PROG)_hpc *.hi *.o *.tix .hpc $(OUTPUT) $(OUTPUT)_hpc $(TESTDATA) $(COVERAGE) bibel.txt
