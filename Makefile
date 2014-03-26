# build Holumbus-Searchengine, Hayoo and W3W

action		= install
hayooFlags	= --flags="hashedIndex hayooSnap4"

all		: install

clean		: ; $(MAKE) target action=clean
configure	: ; $(MAKE) target action=configure
build		: ; $(MAKE) target action=build	
install		: ; $(MAKE) target action=install

target	:
	( cd Holumbus-Searchengine && cabal $(action) )
	( cd Hayoo                 && cabal $(action) $(hayooFlags) )
	( cd W3W                   && cabal $(action) )

init	:
	cabal sandbox init --sandbox .cabal-sandbox
	cabal sandbox add-source Holumbus-Searchengine
	cabal sandbox add-source Hayoo/HayooIndexerLib
	cabal sandbox add-source ../hunt-framework/hunt/hunt-searchengine
	cabal sandbox add-source ../hunt-framework/hayoo/hayooLib
	cd Holumbus-Searchengine \
	  && cabal sandbox init --sandbox    ../.cabal-sandbox
	cd Hayoo/HayooIndexerLib \
	  && cabal sandbox init --sandbox ../../.cabal-sandbox \
	  && cabal sandbox add-source     ../../Holumbus-Searchengine
	cd Hayoo/HayooIndexer \
	  && cabal sandbox init --sandbox ../../.cabal-sandbox \
	  && cabal sandbox add-source     ../../Holumbus-Searchengine \
	  && cabal sandbox add-source     ../../../hunt-framework/hunt/hunt-searchengine \
	  && cabal sandbox add-source     ../../../hunt-framework/hayoo/hayooLib


#	cabal sandbox add-source ../bytestring
#	cabal sandbox add-source ../text
#	cabal sandbox add-source ../data-size
#	cabal sandbox add-source ../data-stringmap
#	cabal sandbox add-source Holumbus-Searchengine
#	cabal sandbox add-source Hayoo/HayooIndexerLib
#	cabal sandbox add-source ../hunt-framework/hunt/hunt-searchengine
#	cabal sandbox add-source ../hunt-framework/hayoo/hayooLib

depends	:
	cabal install --only-dependencies --force-reinstall

delete	:
	cabal sandbox delete

.PHONY	: target clean configure build install all init
