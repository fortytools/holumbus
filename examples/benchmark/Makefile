# Making Benchmark

HOME	= ../..

GHC_FLAGS	= -Wall -O2
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= Benchmark

OUTPUT    = output

all : $(PROG)

start : $(PROG)
	./$(PROG) -i ../indexes/vl-index.bin -d ../indexes/vl-docs.bin

prof : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ -hidir $(OUTPUT) -odir $(OUTPUT) --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(GHC) --make -o $(PROG) $(PROG).hs -hidir $(OUTPUT) -odir $(OUTPUT)

clean :
	$(RM) $(PROG) $(OUTPUT)
	
