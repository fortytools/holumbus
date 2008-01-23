# Making Utilities

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults -fno-warn-orphans -fglasgow-exts
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROGS			= Words Stats

all : $(PROGS)

% : %.hs
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROGS) *.hi *.o Holumbus
	
