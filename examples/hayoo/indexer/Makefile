# Making Hayoo indexer

GHC_FLAGS	= -Wall -O2 -hidir $(OUTPUT) -odir $(OUTPUT)
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= HayooIndexer

OUTPUT    = output

all : $(PROG)

$(PROG) : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) -threaded --make -o $(PROG) $(PROG).hs -i..

clean :
	$(RM) $(PROG) $(OUTPUT)
	
