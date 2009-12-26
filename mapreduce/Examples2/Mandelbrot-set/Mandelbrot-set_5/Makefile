SOURCE		= ./source
OPTIMIZATION=-O2
PROF= #-prof -auto-all -caf-all
GHC_FLAGS	= -threaded $(OPTIMIZATION) -hidir $(OUTPUT) -odir $(OUTPUT) -i$(SOURCE) -main-is main $(PROF) 
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

PROG		= Client Worker

OUTPUT		= output

all : $(PROG)

standalone : WFStandalone

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
