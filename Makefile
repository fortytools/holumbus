VERSION  = 0.1

HDOCFLAGS		= 
HDOC				= haddoc

GHCFLAGS		= -Wall -O2 -hidir ../$(OBJBASE) -odir ../$(OBJBASE)
GHC					= ghc $(GHCFLAGS)

RMFLAGS			= -rf
RM					= rm $(RMFLAGS)

SRCBASE			= src
OBJBASE			= bin

PROG				= $(OBJBASE)/Spoogle
TEST				= $(OBJBASE)/SpoogleTest

MAIN_TEST		= Spoogle/Test/Main.hs
MAIN_PROG		= Spoogle/Main.hs

all					: $(PROG)

test				: $(TEST)
	@echo "===> running Spoogle tests" ; echo ; sleep 2
	$(TEST)

$(TEST)			: 
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_TEST) -o ../$@

$(PROG)			: $(OBJS) $(MAIN_PROG)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_PROG) -o ../$@ 

clean    		:
	$(RM) $(OBJBASE)/*
				 