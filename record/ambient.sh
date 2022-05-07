#!/bin/bash
for i in $(arecord --list-devices | grep USB | cut -d':' -f1 | cut -d' ' -f2)
do
	arecord -f S16_LE -r 44100 --device=hw:$i,0 -c1 ~/Music/ambient_card$(echo $i)_$(date +"%Y%m%d_%H%M%S").wav 2>/dev/null &
done
