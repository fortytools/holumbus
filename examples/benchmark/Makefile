# Making Benchmark

HOME	= ../..

GHC_FLAGS	= -Wall -O2
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= Benchmark

all : $(PROG)

start : $(PROG)
	./$(PROG) -i ../indexes/vl-index.bin -d ../indexes/vl-docs.bin

prof : $(PROG).hs
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ -hidir . -odir . --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	$(GHC) --make -o $(PROG) $(PROG).hs

clean :
	$(RM) $(PROG) *.hi *.o Holumbus
	
