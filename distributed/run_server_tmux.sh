#!/bin/bash
set -e

port=3064
captureBaseLoc='~/Music/pool'
captureLoc=$captureBaseLoc/live
captureCmd="~/Documents/samplescaper/distributed/pool.py $captureBaseLoc"
converterCmd="~/Documents/samplescaper/distributed/cpp/bin/server $port $captureLoc"
localPlayerCmd="~/Documents/samplescaper/singleunit/cpp/player 2 5 $port"
fileListCmd="watch -n1 'ls -l $captureLoc'"

tmuxCmds=()
tmuxCmds+=("tmux new-session \"$captureCmd\"\;")
tmuxCmds+=("split-window -h \"htop\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' \"$converterCmd\"\;")
tmuxCmds+=("select-pane -t 2 \; split-window -v -l '50%' \"$localPlayerCmd\"\;")
tmuxCmds+=("select-pane -t 2 \; split-window -v -l '50%' \"$fileListCmd\"\;")
tmuxCmds+=("select-pane -t 4")

echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
