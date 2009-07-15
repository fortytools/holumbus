# Making Distribution-Standalone Programs

HOME		= ../../

SOURCE		= .
OPTIMIZATION=-O2
PROF=-prof -auto-all -caf-all
GHC_FLAGS	= -threaded $(OPTIMIZATION) -hidir $(OUTPUT) -odir $(OUTPUT) -i$(HOME)$(SOURCE) -main-is main $(PROF)
# -prof -auto-all -caf-all -fforce-recomp
GHC		= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM		= rm $(RM_FLAGS)

PROG		= Mandel ImageTypes ImageMain MandelWorker MandelClient DMandel

OUTPUT		= output

all : $(PROG)

standalone : WFStandalone

#prof : $(PROG).hs
#	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
#	$(RM) $(OUTPUT)/*
#	$(GHC) -prof -auto-all -ignore-package Holumbus -i../../source/ --make -o $(PROG)_p $(PROG).hs

% : %.hs
	[ -d $(OUTPUT) ] || mkdir -p $(OUTPUT)
	$(RM) $(OUTPUT)/*
	$(GHC) --make -o $@ $<

clean :
	$(RM) $(PROG) $(OUTPUT)
	
