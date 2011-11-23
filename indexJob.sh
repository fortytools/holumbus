#! /bin/bash

cd /home/administrator/W3W
mkdir log

echo "Creating index: "`date` >> log/indexJob.log
cd index
make whole 2>&1 >> ../log/indexJob.log

cd ..
echo "Index size: "`du -s ./index | sed -e "s/\(^[^\t]*\)\t.*$/\1/"`" MB" >> log/indexJob.log

echo "Restarting webserver: "`date` >> log/indexJob.log
sudo killall apache2
sudo killall w3wServer
sudo ../.cabal/bin/w3wServer -p 80 > log/out.log 2> log/err.log &