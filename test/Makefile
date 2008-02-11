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

PROG			= AllTests

all : build test

build :
	[ -d $(OUTPUT) ] || mkdir $(OUTPUT)
	$(GHC) --make -o $(PROG) $(PROG).hs

test :
	[ -d $(TESTDATA) ] || mkdir $(TESTDATA)
	@echo "Running Holumbus tests..."
	./$(PROG) -u -q

hpc : clean
	[ -d $(OUTPUT) ] || mkdir $(OUTPUT)
	$(GHC) -fhpc --make -o $(PROG) $(PROG).hs
	[ -d $(TESTDATA) ] || mkdir $(TESTDATA)
	@echo "Gathering coverage..."
	./$(PROG) -u -q
	@echo "Generating markup..."
	@hpc markup $(PROG)
	[ -d $(COVERAGE) ] || mkdir $(COVERAGE)
	@mv *.html $(COVERAGE)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(RM) $(PROG) *.hi *.o .hpc $(OUTPUT) $(TESTDATA) $(COVERAGE)