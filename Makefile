# build Holumbus-Searchengine, Hayoo and W3W

action	= install

all		: install

clean		: ; $(MAKE) target action=clean
configure	: ; $(MAKE) target action=configure
build		: ; $(MAKE) target action=build	
install		: ; $(MAKE) target action=install

target	:
	( cd Holumbus-Searchengine && cabal $(action) )
	( cd Hayoo                 && cabal $(action) )
	( cd W3W                   && cabal $(action) )

.PHONY	: target clean configure build install all
