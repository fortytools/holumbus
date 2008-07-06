# Making Photo Search

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE    = source
OUTPUT    = output

SHADER    = $(SOURCE)/FHW/Search.hs

PROGS     = Indexer

all : $(PROGS)

indexer : Indexer

% : $(SOURCE)/Photo/%.hs output
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

output :
	mkdir -p $(OUTPUT)

clean :
	$(RM) $(OUTPUT) $(PROGS)
