# Making Server

HOME	= ../..

GHC_FLAGS	= -Wall -O2
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= Server

all : $(PROG).o

$(PROG).o : $(PROG).hs
	$(GHC) --make $(PROG).hs

clean :
	$(RM) $(PROG).hi $(PROG).o
	
