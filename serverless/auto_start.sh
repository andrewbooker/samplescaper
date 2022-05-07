#!/bin/bash
if v4l2-ctl --list-devices 2>/dev/null | grep video0
then
    python ~/Documents/remotecamera/server.py 2>/dev/null &
fi

if ! lsusb -v 2>/dev/null | grep Microphone
then
    cd ~/Documents/rotation/
    ./setup.sh home ~/Documents/config.json
    cd ~/Music
    for f in `ls -d 20*`
    do
        ~/Documents/wavmixer/mix.py $f $f/inventory.lof 0.8
        zip -9 -r archives/$f.zip $f
        rm -rf $f
    done
    exit
fi

cd ~/Documents/rotation/
./setup.sh remote ~/Documents/config.json
cd -
~/Documents/samplescaper/record/ambient.sh &
python ~/Documents/rotation/propellorServo.py ~/Documents/config.json &
~/Documents/samplescaper/serverless/start.sh
