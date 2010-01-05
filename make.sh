#!/bin/bash

BASE=`pwd`
PROFILE="--enable-library-profiling --enable-executable-profiling"

function doIt() {
  DIR=$1
  PROF=""
  [ "$2" = "prof" ] && PROF=$PROFILE #"-p --enable-executable-profiling"
  echo $PROF
  cd $DIR && \
#  sudo cabal clean && \
#  sudo cabal install --global $PROF
  runhaskell Setup.hs clean && \
  runhaskell Setup.hs configure $PROF && \
  runhaskell Setup.hs build  && \
  sudo runhaskell Setup.hs install --global
  cd $BASE
}

if [ -z $1 ]
then
  doIt $BASE/distribution/ $2 && \
  doIt $BASE/storage/ $2 && \
  doIt $BASE/mapreduce/ $2
else
  doIt $1 $2
fi
