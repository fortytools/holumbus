#!/bin/bash

DIR=$1
cd $DIR 
mkdir tmp
./Worker $2 logfile.txt +RTS -K4G -N3 -sMemory.txt -RTS 2>> logfile.txt &
echo $! > Worker.pid
