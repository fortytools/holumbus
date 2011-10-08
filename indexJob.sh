#! /bin/bash

cd /home/administrator/W3W
echo "Creating index: "`date` >> indexJob.log
cd index
mv Makefile .. && rm -rf * && mv ../Makefile .
make whole

cd ..
echo "Restarting webserver: "`date` >> indexJob.log
sudo killall webserver
sudo ../.cabal/bin/webserver -p 80 &
