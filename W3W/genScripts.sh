#!/bin/bash

port=${1:-80}
pwd=$(pwd)

cat > ./cronfile <<EOF
0 2 * * * $pwd/indexJob.sh
EOF
echo "created: ./cronfile" 1>&2

cat > ./installCronjob.sh <<EOF
crontab cronfile
EOF
chmod a+x ./installCronjob.sh
echo "created: ./installCronjob.sh" 1>&2

cat > ./indexJob.sh <<EOF
#! /bin/bash

cd $pwd
[ -d "log" ] || mkdir log

echo "Creating index: "\$(date) >> log/indexJob.log
cd index
[ -d "cache" ] || mkdir cache
[ -d "tmp"   ] || mkdir tmp
make whole 2>&1 >> ../log/indexJob.log
cd ..

echo "Restarting webserver: "\$(date) >> log/indexJob.log
sudo killall apache2

sudo killall w3wServer
sleep 2
sudo $HOME/.cabal/bin/w3wServer -p $port > log/out.log 2> log/err.log &
EOF
chmod a+x ./indexJob.sh
echo "created: ./indexJob.sh" 1>&2
