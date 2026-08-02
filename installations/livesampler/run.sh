#!/bin/bash
set -e
../sndaloop.sh

die() {
    echo $1
    exit 1
}
localIp=$(hostname -I | cut -d' ' -f1)
capturePort=3064
captureBaseLoc=/home/$USER/Music/pool
captureLoc=$captureBaseLoc/live
mkdir -p $captureLoc
cmdLoc=/home/$USER/Documents/samplescaper
captureCmd="cd $cmdLoc/distributed; ./pool.py $captureBaseLoc"
converterCmd="$cmdLoc/distributed/cpp/bin/server $capturePort $captureLoc"
fileListCmd="watch -n1 'ls -lth $captureLoc'"

device=$($cmdLoc/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')
[ -n "$device" ] || die "default audio device not available"
playerCmd="cd $cmdLoc/singleunit/cpp; ./run_player.sh 6 $device $capturePort"
recordCmd="arecord -l"

tmuxCmds=()
tmuxCmds+=("tmux new-session bash \;") # capture
tmuxCmds+=("split-window -v -l '84%' htop \;") # player
tmuxCmds+=("select-pane -t 1 \; split-window -h bash \;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '40%' bash \;") # file list
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '60%' bash \;") # converter
tmuxCmds+=("select-pane -t 4 \; split-window -v -l '60%' bash \;") # direct record

tmuxCmds+=("send-keys -t 4 \"$fileListCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 5 \"$playerCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 0 \"$captureCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 2 \"$converterCmd\" ENTER \;")
tmuxCmds+=("send-keys -t 3 \"$recordCmd\" ENTER \;")
tmuxCmds+=("select-pane -t 6")
cmdf=_gen.sh
trap "rm $cmdf" EXIT
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
./$cmdf
