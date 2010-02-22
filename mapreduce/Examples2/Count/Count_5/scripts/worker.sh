#!/bin/bash

echo Hallo >> error.log
[ -n "$1" ] || { echo "Parameter missing: DIR" ; exit 1; }
[ -n "$2" ] || { echo "Parameter missing: PORT"; exit 1; }
[ -n "$2" ] || { echo "Parameter missing: PIDFILE"; exit 1; }

echo Hallo >> error.log

DIR=$1
PIDFILE=$3
cd $DIR 
./Worker $2 logfile.txt +RTS -K4G -sMemory.txt -RTS &
echo $! > $DIR/$PIDFILE
