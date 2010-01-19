GHC_FLAGS = -Wall -threaded -O2 -hidir $(OUTPUT) -odir $(OUTPUT)
GHC       = ghc $(GHC_FLAGS)

RM_FLAGS = -rf
RM       = rm $(RM_FLAGS)

PROG   = StorageControllerDaemon
OUTPUT = output

all : $(PROG)

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
