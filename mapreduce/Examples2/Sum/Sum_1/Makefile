SOURCE		= ./source
OPTIMIZATION=-O2
GHC_FLAGS	= -threaded $(OPTIMIZATION) -hidir $(OUTPUT) -odir $(OUTPUT) -i$(SOURCE) -main-is main
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

PROG		= Worker Client

OUTPUT		= output

all : $(PROG)

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
