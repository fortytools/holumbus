#! /bin/bash

# cd /home/administrator/W3W/index && make whole && killall webserver && cd /home/administrator/W3W && /home/administrator/.cabal/bin/webserver -p 80 > out.txt 2> err.txt &
# echo "Created new index: "`date` >> /home/administrator/W3W/indexJob.log

killall webserver && cd /home/administrator/W3W && /home/administrator/.cabal/bin/webserver -p 80 > out.txt 2> err.txt &
echo "Restarted webserver: "`date` >> /home/administrator/W3W/indexJob.log
