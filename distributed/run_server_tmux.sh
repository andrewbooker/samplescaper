#!/bin/bash
set -e

localIp=192.168.1.88
capturePort=3064
captureBaseLoc='~/Music/pool'
captureLoc=$captureBaseLoc/live
captureCmd="~/Documents/samplescaper/distributed/pool.py $captureBaseLoc"
converterCmd="~/Documents/samplescaper/distributed/cpp/bin/server $capturePort $captureLoc"
fileListCmd="watch -n1 'ls -lth $captureLoc'"
synthCmd="cd ~/Documents/samplecaper/synth/http/ada; ./run.sh 9965"
cameraCmd="~/Documents/mediautils/webcamShow.py 2"


remoteHanging="ssh pi@192.168.1.99"
remoteHangingMotors="cd ~/Documents/samplescaper/singleunit/motors; ./server.py"
remoteHangingPlayer="cd ~/Documents/samplescaper/singleunit/cpp; ./run_player.sh 2 8 $localIp:$capturePort"

remoteBlue="ssh pi@192.168.1.19"
blueClientPlay="~/Documents/samplescaper/distributed/run_client.sh $localIp"
blueClientLog="cd /var/log/randomatones; ./tail_last.sh"

panes=0
tmuxCmds=()
tmuxCmds+=("tmux new-session \"$captureCmd\"\;")
tmuxCmds+=("split-window -h \"$remoteHanging\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '84%' \"$fileListCmd\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '20%' \"$cameraCmd & htop\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v \"$converterCmd\" \;")
topRightPaneIdx=$((${#tmuxCmds[@]}-1))
tmuxCmds+=("select-pane -t $topRightPaneIdx \; split-window -v -l '85%' \"$remoteHanging\" \;")
tmuxCmds+=("select-pane -t $((topRightPaneIdx+1)) \; split-window -v \"$remoteHanging\" \;")
tmuxCmds+=("select-pane -t $((topRightPaneIdx+2)) \; split-window -v \"$remoteBlue\" \;")

tmuxCmds+=("send-keys -t $topRightPaneIdx \"$remoteHangingMotors\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPaneIdx+1)) \"$remoteHangingPlayer\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPaneIdx+2)) \"$blueClientPlay\" ENTER \;")
tmuxCmds+=("send-keys -t $((topRightPaneIdx+3)) \"$blueClientLog\" ENTER \;")

echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
