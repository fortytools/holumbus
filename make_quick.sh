#! /bin/bash

cd ./src && ghc --make -threaded -funbox-strict-fields -O2 -fno-warn-orphans -fno-warn-unused-do-bind -XMultiParamTypeClasses -XTypeSynonymInstances ./Main.hs && sudo cp ./Main ~/.cabal/bin/webserver && ghc --make -threaded -funbox-strict-fields -rtsopts ./W3WIndexer.hs && sudo cp ./W3WIndexer ~/.cabal/bin/w3wIndexer
