# Making FileSystem-Standalone Programs

GHC_FLAGS	= -Wall -threaded -O2 -hidir $(OUTPUT) -odir $(OUTPUT)
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

PROG		= MasterCounter SlaveCounter

OUTPUT		= output

all : $(PROG)

#prof : $(PROG).hs
#	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
#	$(RM) $(OUTPUT)/*
#	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $(PROG)_p $(PROG).hs

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT) *.port
	
