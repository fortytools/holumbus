#! /bin/bash

cd /home/administrator/W3W
echo "Creating index: "`date` >> log/indexJob.log
cd index
mv Makefile .. >> log/indexJob.log
sudo rm -rf *  >> log/indexJob.log
mv ../Makefile . >> log/indexJob.log
make whole >> log/indexJob.log

cd ..
echo "Index size: "`du -s ./index | sed -e "s/\(^[^\t]*\)\t.*$/\1/"`" MB" >> log/indexJob.log

echo "Restarting webserver: "`date` >> log/indexJob.log
sudo killall webserver
sudo ../.cabal/bin/webserver -p 80 > log/out.log 2> log/err.log &
