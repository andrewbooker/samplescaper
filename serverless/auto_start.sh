#!/bin/bash

raspi-gpio set 26 pu
if [ $(raspi-gpio get 26 | sed 's/.*level=\([0-1]\).*/\1/') = 0 ]
then
    cd ~/Documents/rotation/
    ./setup.sh home ~/Documents/config.json
    cd ~/Music
    for f in `ls -d 20*`
    do
        ~/Documents/wavmixer/mix.py $f $f/inventory.lof 0.8 $(jq -r .colour ~/Documents/static.json)
        zip -9 -r archives/$f.zip $f
        rm -rf $f
    done
    exit
fi

cd ~/Documents/rotation/
./setup.sh remote ~/Documents/config.json
cd -
~/Documents/samplescaper/record/ambient.sh
python ~/Documents/rotation/propellorServo.py $(jq .isPilot ~/Documents/static.json) $(jq .propellorRandomInterval ~/Documents/static.json) &
~/Documents/samplescaper/serverless/start.sh
