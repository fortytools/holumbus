#!/bin/bash

BASE=`pwd`
PROFILE="--enable-library-profiling --enable-executable-profiling"

function doIt() {
  DIR=$1

  cd $DIR && \
  runhaskell Setup.hs configure $PROFILE && \
  runhaskell Setup.hs build  && \
  sudo runhaskell Setup.hs install --global
  cd $BASE
}

if [ -z $1 ]
then
  doIt $BASE/distribution/ && \
  doIt $BASE/storage/ && \
  doIt $BASE/mapreduce/
else
  doIt $1
fi
