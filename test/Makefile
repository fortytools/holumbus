# Making all tests

HOME			= ..

# Make GHC compile everything seperatly for the tests, to be able to 
# execute them without installing the library first.
GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults -fno-warn-orphans -ignore-package Holumbus -i$(SOURCE) -odir . -hidir .
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE		= ../source

PROG			= AllTests

all : test

test :
	$(GHC) --make -o $(PROG) $(PROG).hs
	@echo "Running Holumbus tests..."
	./$(PROG)

clean :
	$(RM) $(PROG) *.hi *.o Holumbus