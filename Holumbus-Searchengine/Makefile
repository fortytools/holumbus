# initialize cabal sandbox (cabal-1.18 required)
# after this init perform a normal "cabal build"

init	:
	cabal sandbox init
	cabal sandbox add-source ../../bytestring
	cabal sandbox add-source ../../text
	cabal sandbox add-source ../../data-size
	cabal sandbox add-source ../../data-stringmap
	cabal install --only-dependencies --force-reinstall --enable-tests

delete  :
	cabal sandbox delete

unregister	:
	cabal sandbox hc-pkg unregister Holumbus-Searchengine
	cabal sandbox hc-pkg unregister data-stringmap
	cabal sandbox hc-pkg unregister data-size
