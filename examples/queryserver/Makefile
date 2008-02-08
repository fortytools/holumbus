# Making Server

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -threaded -hidir $(OUTPUT) -odir $(OUTPUT)
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= Server

OUTPUT    = output

all : $(PROG)

start : $(PROG)
	./$(PROG) -i ../indexes/vl-index.bin

prof : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) --make -o $(PROG) $(PROG).hs

clean :
	$(RM) $(PROG) $(OUTPUT)
	
