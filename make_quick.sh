#! /bin/bash

cd ./src
ghc --make -XTypeSynonymInstances -XMultiParamTypeClasses ./Main.hs && cp ./Main .. && cd .. && ./Main 
