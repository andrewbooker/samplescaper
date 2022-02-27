#!/bin/bash

if lsusb -v 2>/dev/null | grep bInterfaceProtocol | grep Keyboard
then
    cd ~/Music
    for f in `ls -d 20*`
    do
        ~/Documents/wavmixer/mix.py $f $f/inventory.lof 0.8
        zip -9 -r archives/$f.zip $f
        rm -rf $f
    done
    exit
fi

~/Documents/samplescaper/record/ambient.sh &
python ~/Documents/rotation/motor.py ~/Documents/config.json &
~/Documents/samplescaper/serverless/start.sh
