VERSION  = 0.1

GHCFLAGS = -Wall -O2
GHC      = ghc $(GHCFLAGS)

prog     = ./SpoogleTest

all      : $(prog)

force    :
	$(GHC) -o $(prog) $(prog).hs

$(prog)  : $(prog).hs
	$(GHC) -o $@ $<

test     : $(prog)
	@echo "===> running Spoogle tests" ; echo ; slepp2
	$(prog)

clean    :
	rm -f $(prog) $(prog).o $(prog).hi
				 