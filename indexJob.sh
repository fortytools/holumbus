#! /bin/bash

cd /home/administrator/W3W
echo "Creating index: "`date` >> indexJob.log
cd index
mv Makefile .. && rm -rf * && mv ../Makefile .
make whole

cd ..
echo "Index size: "`du -s ./index | sed -e "s/\(^[^\t]*\)\t.*$/\1/"`" MB" >> indexJob.log

echo "Restarting webserver: "`date` >> indexJob.log
sudo killall webserver
sudo ../.cabal/bin/webserver -p 80 &
