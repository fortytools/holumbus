#!/bin/bash

DIR=$1
cd $DIR 
./Worker $2 logfile +RTS -K4G -N3 -sMemory.txt -RTS &
echo $! > Worker.pid
