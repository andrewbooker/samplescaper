#!/bin/bash

if lsusb -v 2>/dev/null | grep bInterfaceProtocol | grep Keyboard
then
    cd ~/Music
    for f in `ls -d 20*`
    do
        ~/Documents/samplescaper/record/assembleAudio.py ~/Music/$f
	zip -9 -r archives/$f.zip $f
	rm -rf $f
    done
    exit
fi


rm -rf ~/Music/pool
rm -rf ~/Music/samples
mkdir -p ~/Music/archives
mkdir -p ~/Music/samples
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

tonic=48

#python ~/Documents/samplescaper/synth/modify.py ~/Music/samples ~/Music/pool/raw &
#python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/raw ~/Music/samples &
python ~/Documents/rotation/motor.py $tonic &
python ~/Documents/samplescaper/synth/synth.py ~/Music/pool $tonic &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool ~/Documents/samplescaper/serverless/config.json &
~/Documents/samplescaper/record/ambient.sh &
