#!/bin/bash

raspi-gpio set 19 pu
if [ $(raspi-gpio get 19 | sed 's/.*level=\([0-1]\).*/\1/') = 0 ]
then
    raspi-gpio set 26 op
    raspi-gpio set 26 dh
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

cd ~/Documents/rotation/
./setup.sh remote ~/Documents/config.json
cd -
~/Documents/samplescaper/record/ambient.sh
python ~/Documents/samplescaper/serverless/detect_auto.py &
python ~/Documents/rotation/propellorServo.py $(jq .isPilot ~/Documents/static.json) &
~/Documents/samplescaper/serverless/start.sh
