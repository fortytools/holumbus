# Making all tests

HOME			= ..

# Make GHC compile everything seperatly for the tests, to be able to 
# execute them without installing the library first.
GHC_FLAGS	= -Wall -O2 -threaded -ignore-package Holumbus -i$(SOURCE) -odir $(OUTPUT) -hidir $(OUTPUT)
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE		= ../source

TESTDATA  = data
OUTPUT    = output
COVERAGE  = coverage

TESTS     = BinaryTest CrunchTest DocumentsTest FuzzyTest InvertedTest \
            ParserTest PickleTest StrMapTest DiffListTest

PROG			= AllTests

all : build test

build :
	[ -d $(OUTPUT) ] || mkdir $(OUTPUT)
	$(GHC) -odir $(OUTPUT) -hidir $(OUTPUT) --make -o $(PROG) $(PROG).hs

test :
	[ -d $(TESTDATA) ] || mkdir $(TESTDATA)
	@echo "Running Holumbus tests..."
	./$(PROG) -u -q

hpc :
	[ -d $(OUTPUT)_hpc ] || mkdir $(OUTPUT)_hpc
	$(GHC) -odir $(OUTPUT)_hpc -hidir $(OUTPUT)_hpc -fhpc --make -o $(PROG)_hpc $(PROG).hs
	[ -d $(TESTDATA) ] || mkdir $(TESTDATA)
	@echo "Gathering coverage..."
	./$(PROG)_hpc -u -q
	[ -d $(COVERAGE) ] || mkdir $(COVERAGE)
	@echo "Generating markup..."
	@hpc markup --destdir=$(COVERAGE) $(TESTS:%=--exclude=%) --exclude=Main --exclude=SampleData $(PROG)_hpc

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(RM) $(PROG) $(PROG)_hpc *.hi *.o *.tix .hpc $(OUTPUT) $(OUTPUT)_hpc $(TESTDATA) $(COVERAGE)