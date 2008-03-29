# Making Hebalizer

HOME	= ../..

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= Hebalizer

OUTPUT    = output

all : $(PROG)

prof : $(PROG:%=%_p)

%_p :%.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

% : %.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@ 
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

clean :
	$(RM) $(PROG) $(PROG:%=%_p) $(OUTPUT)
	
