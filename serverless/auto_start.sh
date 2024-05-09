#!/bin/bash

LED=14
BUTTON=15
raspi-gpio set $BUTTON pu
if [ $(raspi-gpio get $BUTTON | sed 's/.*level=\([0-1]\).*/\1/') = 0 ]
then
    raspi-gpio set $LED op
    raspi-gpio set $LED dh
    cd ~/Documents/rotation/
    ./setup.sh home ~/Documents/config.json
    cd ~/Music
    for f in `ls -d 20*`
    do
        ~/Documents/wavmixer/mix.py $f $f/inventory.lof 0.7 $(jq -r .colour ~/Documents/static.json)
        zip -9 -r archives/$f.zip $f
        rm -rf $f
    done
    exit
fi

sudo swapoff -a
cd ~/Documents/rotation/
./setup.sh remote ~/Documents/config.json
cd -
~/Documents/samplescaper/record/ambient.sh
python ~/Documents/samplescaper/record/remoteCtlCamera.py &
python ~/Documents/samplescaper/serverless/detect_auto.py $LED $BUTTON &
python ~/Documents/rotation/propellorServo.py $(jq .isPilot ~/Documents/static.json) &
~/Documents/samplescaper/serverless/start.sh
