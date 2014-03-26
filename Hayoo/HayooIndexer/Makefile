isghc77	= ghc --version | sed -e 's|^[^0-9]*||' | grep -q -e '^7.[789]'

all	:
	@echo "available targets: init depends delete unregister"

init	:
	@echo "initialize sandbox in ~/haskell/holumbus: make -C ../.. init"

#	cabal sandbox init
#	$(isghc77) || \
#	cabal sandbox add-source ../../../bytestring
#	cabal sandbox add-source ../../../data-size
#	cabal sandbox add-source ../../../data-stringmap
#	@echo NOT ADDED: cabal sandbox add-source ../../../ghc-heap-view
#	cabal sandbox add-source ../../../text
#	cabal sandbox add-source ../../Holumbus-Searchengine
#	cabal sandbox add-source ../HayooIndexerLib
#	cabal sandbox add-source ../../../hunt-framework/hunt/hunt-searchengine

depends	:
	cabal install --dependencies-only --enable-tests --force-reinstall

delete	:
	@echo "remove common sandbox in ~/haskell/holumbus/ manually"

unregister	:
	cabal sandbox hc-pkg unregister hunt-searchengine
	cabal sandbox hc-pkg unregister Hayoo-Indexer-Lib
	cabal sandbox hc-pkg unregister Holumbus-Searchengine
	cabal sandbox hc-pkg unregister data-stringmap
	cabal sandbox hc-pkg unregister data-size

.PHONY	: init delete unregister depends
