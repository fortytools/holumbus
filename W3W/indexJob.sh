#! /bin/bash

cd /home/administrator/W3W
mkdir log

echo "Creating index: "`date` >> log/indexJob.log
cd index
make whole 2>&1 >> ../log/indexJob.log
cd ..

echo "Restarting webserver: "`date` >> log/indexJob.log
sudo killall apache2
sudo killall w3wServer
sudo ../.cabal/bin/w3wServer -p 80 > log/out.log 2> log/err.log &