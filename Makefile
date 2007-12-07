NAME					= Spoogle
DESCR					= A distributed search and indexing engine
VERSION  			= 0.1

HDOCFLAGS			= -v -h -o $(DOCBASE) -t "$(NAME)-$(VERSION): $(DESCR)" --use-package=base
HDOC					= haddock $(HDOCFLAGS)

GHCFLAGS			= -Wall -fno-warn-type-defaults -O2 -hidir ../$(OBJBASE) -odir ../$(OBJBASE)
GHC						= ghc $(GHCFLAGS)

RMFLAGS				= -rf
RM						= rm $(RMFLAGS)
	
SRCBASE				= src
OBJBASE				= bin
DOCBASE				= doc

PRG_SEARCH		= $(OBJBASE)/SpoogleSearch
PRG_CRAWL			= $(OBJBASE)/SpoogleCrawl
PRG_TEST			= $(OBJBASE)/SpoogleTest

MAIN_TEST			= Spoogle/Test/Main.hs
MAIN_SEARCH		= Spoogle/Query/Main.hs
MAIN_CRAWL		= Spoogle/Crawl/Main.hs

SRC_ALL				= $(wildcard ./$(SRCBASE)/Spoogle/Data/*.hs) \
	$(wildcard ./$(SRCBASE)/Spoogle/Index/*.hs) \
	$(wildcard ./$(SRCBASE)/Spoogle/Query/*.hs) \
	$(wildcard ./$(SRCBASE)/Spoogle/Crawl/*.hs) \
	$(wildcard ./$(SRCBASE)/Spoogle/Test/*.hs)

all						: search # crawl

doc						: $(SRC_ALL)
	[ -d $(DOCBASE) ] || mkdir -p ./$(DOCBASE)
	$(HDOC) $(SRC_ALL)

version				:
	@echo $(NAME)-$(VERSION): $(DESCR)

test					: $(PRG_TEST)
	@echo "===> running Spoogle tests" ; echo ; sleep 2
	$(PRG_TEST)

search				: $(PRG_SEARCH)
crawl					: $(PRG_CRAWL)

$(PRG_TEST)		: $(SRCBASE)/$(MAIN_TEST) $(SRC_ALL)
	[ -d $(OBJBASE) ] || mkdir -p $(OBJBASE)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_TEST) -o ../$@

$(PRG_SEARCH)	: $(SRCBASE)/$(MAIN_SEARCH) $(SRC_ALL)
	[ -d $(OBJBASE) ] || mkdir -p $(OBJBASE)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_SEARCH) -o ../$@ 

$(PRG_CRAWL)	: $(SRCBASE)/$(MAIN_CRAWL) $(SRC_ALL)
	[ -d $(OBJBASE) ] || mkdir -p $(OBJBASE)
	cd $(SRCBASE) ; $(GHC) --make $(MAIN_CRAWL) -o ../$@ 

clean    			:
	$(RM) $(OBJBASE) $(DOCBASE)
				 