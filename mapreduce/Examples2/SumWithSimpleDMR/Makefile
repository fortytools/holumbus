HOME		= ../../

SOURCE		= .
OPTIMIZATION=-O2
GHC_FLAGS	= -threaded $(OPTIMIZATION) -hidir $(OUTPUT) -odir $(OUTPUT) -i$(HOME)$(SOURCE) -main-is main
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

PROG		= Sum SumWorker SumClient Standalone

OUTPUT		= output

all : $(PROG)

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
