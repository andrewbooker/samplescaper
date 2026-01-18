#!/bin/bash
set -e

localIp=$(hostname -I | cut -d' ' -f1)
capturePort=3064
captureBaseLoc='~/Music/pool'
captureLoc=$captureBaseLoc/live
captureCmd="~/Documents/samplescaper/distributed/pool.py $captureBaseLoc"
converterCmd="~/Documents/samplescaper/distributed/cpp/bin/server $capturePort $captureLoc"
fileListCmd="watch -n1 'ls -lth $captureLoc'"
synth1Cmd="cd ~/Documents/samplescaper/synth/http/ada; ./run.sh 9964"
synth2Cmd="cd ~/Documents/samplescaper/synth/http/go; ./run.sh 9965"
cameraCmd="~/Documents/mediautils/webcamShow.py 2"

remoteHanging="ssh pi@192.168.1.99"
remoteHangingMotors="cd ~/Documents/samplescaper/singleunit/motors; ./server.py"
remoteHangingPlayer="cd ~/Documents/samplescaper/singleunit/cpp; ./run_player.sh 2 7 $localIp:9964 $localIp:9965"

remoteBlue="ssh pi@192.168.1.19"
blueClientPlay="~/Documents/samplescaper/distributed/run_client.sh $localIp"
blueClientLog="cd /var/log/randomatones; sleep 2; ./tail_last.sh"

panes=0
tmuxCmds=()
tmuxCmds+=("tmux new-session \"$captureCmd\"\;")
tmuxCmds+=("split-window -h \"$remoteHanging\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '84%' \"$cameraCmd & $fileListCmd\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '33%' \"$synth1Cmd\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '50%' \"$synth2Cmd\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v \"$converterCmd\" \;")
topRightPane=$((${#tmuxCmds[@]}-1))
tmuxCmds+=("select-pane -t $topRightPane \; split-window -v -l '85%' \"$remoteHanging\" \;")
tmuxCmds+=("select-pane -t $((topRightPane+1)) \; split-window -v \"$remoteBlue\" \;")
tmuxCmds+=("select-pane -t $((topRightPane+2)) \; split-window -v \"$remoteBlue\" \;")

tmuxCmds+=("send-keys -t $topRightPane \"$remoteHangingMotors\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPane+1)) \"$remoteHangingPlayer\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPane+3)) \"$blueClientPlay\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPane+2)) \"$blueClientLog\" ENTER \;")

cmdf=_gen1.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
./$cmdf
rm $cmdf
