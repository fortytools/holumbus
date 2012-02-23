#!/bin/bash

pwd=$(pwd)

cat > ./cronfile <<EOF
0 2 * * * $pwd/indexJob.sh
EOF

cat > ./installCronjob.sh <<EOF
crontab cronfile
EOF
chmod a+x ./installCronjob.sh

cat > ./indexJob.sh <<EOF
#! /bin/bash

cd $pwd
[ -d "log" ] || mkdir log

echo "Creating index: "\$(date) >> log/indexJob.log
cd index
make whole 2>&1 >> ../log/indexJob.log
cd ..

echo "Restarting webserver: "\$(date) >> log/indexJob.log
sudo killall apache2

sudo killall w3wServer
sleep 2
sudo $HOME/.cabal/bin/w3wServer -p 80 > log/out.log 2> log/err.log &
EOF
chmod a+x ./indexJob.sh