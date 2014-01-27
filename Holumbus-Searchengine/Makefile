# initialize cabal sandbox (cabal-1.18 required)
# after this init perform a normal "cabal build"

isghc77	= ghc --version | sed -e 's|^[^0-9]*||' | grep -q -e '^7.[789]'

all	:
	@echo "targets: init depends delete unregister"

init	:
	cabal sandbox init
	$(isghc77) || cabal sandbox add-source ../../bytestring
	cabal sandbox add-source ../../text
	cabal sandbox add-source ../../data-size
	cabal sandbox add-source ../../data-stringmap

depends	:
	cabal install --only-dependencies --force-reinstall --enable-tests

delete  :
	cabal sandbox delete

unregister	:
	cabal sandbox hc-pkg unregister Holumbus-Searchengine
	cabal sandbox hc-pkg unregister data-stringmap
	cabal sandbox hc-pkg unregister data-size

.PHONY	: init depends delete unregister
