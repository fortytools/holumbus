# Making Utilities

HOME	= ../..

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROGS			= Words Stats Convert Split Merge Update Info

OUTPUT    = output

all : $(PROGS)

prof : $(PROGS:%=%_p)

%_p :%.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@
	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

% : %.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@ 
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

clean :
	$(RM) $(PROGS) $(PROGS:%=%_p) $(OUTPUT)
	
