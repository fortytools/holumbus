isghc77	= ghc --version | sed -e 's|^[^0-9]*||' | grep -q -e '^7.[789]'

all	:
	@echo "available targets: init depends delete"

init	:
	cabal sandbox init
	$(isghc77) || \
        cabal sandbox add-source ../../../bytestring
	cabal sandbox add-source ../../../data-size
	cabal sandbox add-source ../../../data-stringmap
	@echo NOT ADDED: cabal sandbox add-source ../../../ghc-heap-view
	cabal sandbox add-source ../../../text
	cabal sandbox add-source ../../Holumbus-Searchengine
	cabal sandbox add-source ../HayooLib

depends	:
	cabal install --dependencies-only --enable-tests --force-reinstall

delete	:
	cabal sandbox delete

.PHONY	: all init depends delete
