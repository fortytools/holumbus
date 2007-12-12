# Making all tests

HOME			= ..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= AllTests

all : test

test :
	$(GHC) --make -o $(PROG) $(PROG).hs
	@echo "Running Spoogle tests..."
	./$(PROG)

clean :
	$(RM) $(PROG) $(PROG).hi $(PROG).o