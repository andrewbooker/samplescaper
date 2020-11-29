#!/bin/bash
device=`arecord --list-devices | grep USB`
if [ ! -z "$device" ]
then
    arecord -f S16_LE -r 44100 --device=hw:1,0 -c1  ~/Music/ambient$(date +"%Y%m%d_%H%M%S").wav
fi
