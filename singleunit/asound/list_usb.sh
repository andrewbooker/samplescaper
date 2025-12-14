#!/bin/bash
for i in /proc/asound/card*/pcm*p/info
do
    card=$(cat $i | grep 'card:' | cut -d' ' -f2)
    if [[ $(cat $i | grep USB) != '' ]]
    then
        echo $i | cut -d'/' -f4
    fi
done
