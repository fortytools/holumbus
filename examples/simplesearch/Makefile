# Making SimpleSearch

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= SimpleSearch

all : 
	$(GHC) --make -o $(PROG) $(PROG).hs

clean :
	$(RM) $(PROG) $(PROG).hi $(PROG).o
	