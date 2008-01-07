# Making WebSearch

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults -farrows
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= WebSearch

all : $(PROG).o

$(PROG).o : $(PROG).hs
	$(GHC) --make $(PROG).hs

clean :
	$(RM) $(PROG).hi $(PROG).o
	
