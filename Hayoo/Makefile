# initialize cabal sandbox (cabal-1.18 required)
# after this init perform a normal "cabal build"
# "cabal install" will install Hayoo indexer and server in ./.cabal-sandbox/bin

init	:
	cabal sandbox init
	cabal sandbox add-source ../../bytestring
	cabal sandbox add-source ../../text
	cabal sandbox add-source ../../data-size
	cabal sandbox add-source ../../data-stringmap
	cabal sandbox add-source ../Holumbus-Searchengine
	cabal sandbox add-source ../../hunt-framework/hunt/hunt-searchengine
	cabal install --only-dependencies --force-reinstall

delete  :
	cabal sandbox delete

unregister	:
	cabal sandbox hc-pkg unregister hunt-searchengine
	cabal sandbox hc-pkg unregister Holumbus-Searchengine
	cabal sandbox hc-pkg unregister data-stringmap
	cabal sandbox hc-pkg unregister data-size
