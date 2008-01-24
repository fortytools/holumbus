# Making Utilities

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults -fno-warn-orphans -fglasgow-exts
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROGS			= Words Stats Convert

OUTPUT    = output

all : $(PROGS)

% : %.hs
	[ -d $(OUTPUT)/$@ ] || mkdir -p $(OUTPUT)/$@
	$(GHC) --make -o $@ $< -hidir $(OUTPUT)/$@ -odir $(OUTPUT)/$@

clean :
	$(RM) $(PROGS) *.hi *.o $(OUTPUT)
	
