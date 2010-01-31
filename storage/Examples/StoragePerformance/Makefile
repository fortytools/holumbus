GHC_FLAGS = -threaded -O2 -hidir $(OUTPUT) -odir $(OUTPUT) -XScopedTypeVariables
GHC       = ghc $(GHC_FLAGS)

RM_FLAGS = -rf
RM       = rm $(RM_FLAGS)

PROG   = NetFile_1  ReadFile_1 NetFile_2 ReadFile_2 
OUTPUT = output

all : $(PROG)

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
