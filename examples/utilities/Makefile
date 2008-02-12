# Making Utilities

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -threaded
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROGS			= Words Stats Convert Split Merge Update Info

OUTPUT    = output

all : $(PROGS)

% : %.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@
	$(GHC) --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

clean :
	$(RM) $(PROGS) $(OUTPUT)
	
