VERSION  = 0.1

HDOCFLAGS		= 
HDOC				= haddock

GHCFLAGS		= -Wall -fno-warn-type-defaults -O2 -hidir ../$(OBJBASE) -odir ../$(OBJBASE)
GHC					= ghc $(GHCFLAGS)

RMFLAGS			= -rf
RM					= rm $(RMFLAGS)

SRCBASE			= src
OBJBASE			= bin

PROG				= $(OBJBASE)/SpoogleSearch
TEST				= $(OBJBASE)/SpoogleTest

MAIN_TEST		= Spoogle/Test/Main.hs
MAIN_PROG		= Spoogle/Main.hs

all					: search

test				: $(TEST)
	@echo "===> running Spoogle tests" ; echo ; sleep 2
	$(TEST)

search			: $(PROG)

$(TEST)			: $(SRCBASE)/$(MAIN_TEST)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_TEST) -o ../$@

$(PROG)			: $(SRCBASE)/$(MAIN_PROG)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_PROG) -o ../$@ 

clean    		:
	$(RM) $(OBJBASE)/*
				 