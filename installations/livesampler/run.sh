#!/bin/bash
set -e

localIp=$(hostname -I | cut -d' ' -f1)
capturePort=3064
captureBaseLoc=/home/$USER/Music/pool
captureLoc=$captureBaseLoc/live
mkdir -p $captureLoc
cmdLoc=/home/$USER/Documents/samplescaper
captureCmd="cd $cmdLoc/distributed; ./pool.py $captureBaseLoc"
converterCmd="$cmdLoc/distributed/cpp/bin/server $capturePort $captureLoc"
fileListCmd="watch -n1 'ls -lth $captureLoc'"

playerCmd="cd $cmdLoc/singleunit/cpp; ./run_player.sh 2 7 $capturePort"
recordCmd="arecord -l"

tmuxCmds=()
tmuxCmds+=("tmux new-session \"htop\" \;")
tmuxCmds+=("split-window -h \"bash\" \;") # player
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '70%' \"bash\" \;") # file list
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '80%' \"bash\" \;") # converter
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '50%' \"bash\" \;") # capture
tmuxCmds+=("select-pane -t 2 \; split-window -v -l '50%' \"bash\" \;") # direct record

topRightPane=$((${#tmuxCmds[@]}-2))
tmuxCmds+=("send-keys -t $((topRightPane+1)) \"$playerCmd\" ENTER \;")
tmuxCmds+=("send-keys -t $topRightPane \"$fileListCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 1 \"$captureCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 2 \"$converterCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 3 \"$recordCmd\" ENTER \;")
tmuxCmds+=("select-pane -t $((topRightPane+1))")


cmdf=_gen1.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
./$cmdf
rm $cmdf
