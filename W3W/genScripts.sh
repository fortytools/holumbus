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

# set -x

index=1
[ "\$1" == "-r" ] && index=0

cd $pwd
[ -d "log" ] || mkdir log

function indexing() {
  ( echo "Creating index: "\$(date)
    cd index
    [ -d "cache" ] || mkdir cache
    [ -d "tmp"   ] || mkdir tmp
    make whole
  ) 1>&2
}

function restart() {
  ( echo "Restarting webserver: "\$(date)
    # apache2 is started at boot time
    sudo killall apache2   || echo "no apache2   process killed"
    sudo killall w3wServer || echo "no w3wServer process killed"
    sleep 2
    ps axf | egrep -e '(apache2|w3wServer)'
    sudo nohup \$HOME/.cabal/bin/w3wServer -p $port 1>&2 < /dev/null &
    sleep 2
    ps axf | egrep -e 'w3wServer'
  ) 1>&2
}

{ if [[ "\$index" -eq "1" ]]
    then
    indexing
  else
    echo "indexing skipped"
  fi
  restart
} > log/indexJob.log 2>&1
EOF
chmod a+x ./indexJob.sh
echo "created: ./indexJob.sh" 1>&2
