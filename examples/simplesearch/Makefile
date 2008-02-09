# Making SimpleSearch

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -hidir $(OUTPUT) -odir $(OUTPUT)
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= SimpleSearch

OUTPUT    = output

all : $(PROG)

start : $(PROG)
	./$(PROG) -i ../indexes/sd-index.bin -d ../indexes/sd-docs.bin

prof : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) -threaded --make -o $(PROG) $(PROG).hs

clean :
	$(RM) $(PROG) $(OUTPUT)
	
