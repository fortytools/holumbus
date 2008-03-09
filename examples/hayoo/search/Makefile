# Making Hayoo indexer

GHC_FLAGS	= -Wall -O2
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= HayooSearch

OUTPUT    = output

all : $(PROG)

$(PROG) : $(PROG).hs
	$(GHC) --make $(PROG).hs -i..

clean :
	$(RM) $(PROG).o $(PROG).hi
	
