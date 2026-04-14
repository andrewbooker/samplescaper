#!/bin/bash
set -e

if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi
synths=(go rust)
basePort=9960
base="/home/$USER/Documents/samplescaper"
synthBase="$base/synth/http"
playerBase="$base/singleunit/cpp"

playerCmd="./run_player.sh 6 23 $basePort"
mpxCmd="./run.sh $basePort"

synthCmd () {
    language=$1
    port=$2
    echo "cd $synthBase/$language; ./run.sh $port"
}

tmuxCmds=()
tmuxCmds+=("tmux new-session \"htop\" \;")
tmuxCmds+=("split-window -h \"$(synthCmd haskell 9961)\" \;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '82%' \"cd $playerBase; $playerCmd\" \;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '33%' \"cd $playerBase/multiplexer; $mpxCmd\" \;")

topRightPane=$((${#tmuxCmds[@]}-1))
tmuxCmds+=("select-pane -t $topRightPane \; split-window -v -l '50%' \"$(synthCmd go 9962)\" \;")
tmuxCmds+=("select-pane -t 1")

cmdf=gen_inst.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
trap "rm $cmdf" EXIT
./$cmdf


