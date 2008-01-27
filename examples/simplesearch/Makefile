# Making SimpleSearch

HOME	= ../..

GHC_FLAGS	= -Wall -O2
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= SimpleSearch

all : $(PROG)

start : $(PROG)
	./$(PROG) -i ../indexes/sd-index.bin -d ../indexes/sd-docs.bin

prof : $(PROG).hs
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ -hidir . -odir . --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	$(GHC) --make -o $(PROG) $(PROG).hs 

clean :
	$(RM) $(PROG) *.hi *.o Holumbus
	
