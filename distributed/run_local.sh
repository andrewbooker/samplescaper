#!/bin/bash
set -e

capturePort=3064
captureBaseLoc='~/Music/pool'
captureLoc=$captureBaseLoc/live
deviceName='USB Audio CODEC'
#deviceName='front, ALSA'
device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) front, ALSA.*/\1/p')

captureCmd="~/Documents/samplescaper/distributed/pool.py $captureBaseLoc"
converterCmd="~/Documents/samplescaper/distributed/cpp/bin/server $capturePort $captureLoc"
fileListCmd="watch -n1 'ls -lth $captureLoc'"
localPlayer="cd ~/Documents/samplescaper/singleunit/cpp; ./run_player.sh 2 device 3064"

panes=0
tmuxCmds=()
tmuxCmds+=("tmux new-session \"$captureCmd\"\;")
tmuxCmds+=("split-window -h \"$localPlayer\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '84%' \"$fileListCmd\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '33%' \"htop\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v \"$converterCmd\" \;")
topRightPane=$((${#tmuxCmds[@]}-1))

cmdf=_gen3.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
./$cmdf
rm $cmdf
