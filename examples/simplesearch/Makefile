# Making SimpleSearch

HOME	= ../..

GHC_FLAGS	= -Wall -O2 -fno-warn-type-defaults -fglasgow-exts
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

PROG			= SimpleSearch

all : $(PROG)

start : $(PROG)
	./$(PROG) -i sd.xml

prof : $(PROG).hs
	$(GHC) -prof -auto-all --make -o $(PROG) $(PROG).hs

$(PROG) : $(PROG).hs
	$(GHC) --make -o $(PROG) $(PROG).hs

clean :
	$(RM) $(PROG) $(PROG).hi $(PROG).o
	
