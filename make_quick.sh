#! /bin/bash

cd ./src
ghc --make -XTypeSynonymInstances -XMultiParamTypeClasses ./Main.hs && cp ./Main ~/.cabal/bin/webserver
ghc --make ./W3WIndexer.hs && cp ./W3WIndexer ~/.cabal/bin/w3wIndexer
