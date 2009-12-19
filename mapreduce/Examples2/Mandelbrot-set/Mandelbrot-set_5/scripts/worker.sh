#!/bin/bash

echo Hallo >> error.log
[ -n "$1" ] || { echo "$0 Parameter missing: DIR" ; exit 1; }
[ -n "$2" ] || { echo "$0 Parameter missing: PORT"; exit 1; }
[ -n "$2" ] || { echo "$0 Parameter missing: PIDFILE"; exit 1; }

echo Hallo >> error.log

DIR=$1
PIDFILE=$3
cd $DIR 
./Worker $2 logfile.txt +RTS -K4G -N3 -sMemory.txt -RTS &
echo $! > $DIR/$PIDFILE
