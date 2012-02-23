#! /bin/bash

cd /home/theo/haskell/holumbus/W3W
[ -d "log" ] || mkdir log

echo "Creating index: "$(date) >> log/indexJob.log
cd index
make whole 2>&1 >> ../log/indexJob.log
cd ..

echo "Restarting webserver: "$(date) >> log/indexJob.log
sudo killall apache2

sudo killall w3wServer
sleep 2
sudo /home/theo/.cabal/bin/w3wServer -p 80 > log/out.log 2> log/err.log &
