# Making all tests

HOME			= ..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= AllTests

all : test

test : $(PROG)
	@echo "Running Spoogle tests..."
	./$(PROG)

$(PROG) : $(PROG).hs
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(PROG).hi $(PROG).o