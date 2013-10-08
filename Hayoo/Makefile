# initialize cabal sandbox (cabal-1.18 required)
# after this init perform a normal "cabal build"
# "cabal install" will install Hayoo indexer and server in ./.cabal-sandbox/bin

init	:
	cabal sandbox init
	cabal sandbox add-source ../Holumbus-Searchengine
	cabal install --only-dependencies
