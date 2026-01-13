#!/bin/bash
sudo swapoff -a
value_of() {
    raspi-gpio get $1 | sed 's/.*level=\([0-1]\).*/\1/'
}

LED=14
BUTTON=15
JUMP=26
raspi-gpio set $BUTTON pu
raspi-gpio set $JUMP pu

if [ $(value_of $BUTTON) = 0 ]; then
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
elif [ $(value_of $JUMP) = 0 ]; then
    rplog='/var/log/randomatones/remote_player_startup.log'
    echo $(date) > $rplog
    echo 'starting in distributed remote-player mode' >> $rplog
    cd ~/Documents/rotation/
    ./setup.sh remote ~/Documents/config.json
    cd -
    sleep 3
    ipa=$(hostname -I)
    echo "IP address ${ipa}" >> $rplog
    sudo wpa_cli -i wlan0 list_networks >> $rplog
    python ~/Documents/rotation/propellorServo.py 0 &
    echo "motors started" >> $rplog
else
    cd ~/Documents/rotation/
    ./setup.sh remote ~/Documents/config.json
    cd -
    ~/Documents/samplescaper/record/ambient.sh
    python ~/Documents/samplescaper/serverless/detect_auto.py $LED $BUTTON &
    python ~/Documents/rotation/propellorServo.py $(jq .isPilot ~/Documents/static.json) $(jq .tonic ~/Documents/samplescaper/key.json) &
    python ~/Documents/samplescaper/utils/rgbselector/rgb.py &
    ~/Documents/samplescaper/serverless/start.sh
fi



