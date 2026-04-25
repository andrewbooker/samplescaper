#!/bin/bash
set -e

if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi
synths=(go rust ada pascal)
basePort=9960
device=23
channels=4
base="/home/$USER/Documents/samplescaper"
synthBase="$base/synth/http"
playerBase="$base/singleunit/cpp"

playerCmd="./run_player.sh $channels $device $basePort"
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
for s in ${synths[@]}
do
    tmuxCmds+=("select-pane -t $topRightPane \; split-window -v -l '20%' \"cd $synthBase/$s; bash\" \;")
done

for ((i=1; i <= ${#synths[@]}; ++i))
do
    tmuxCmds+=("select-pane -t $((topRightPane+i)) \; send-keys \"sleep $((10+i)); ./run.sh $((basePort+1+i))\" ENTER \;")
done
tmuxCmds+=("select-pane -t 1")

cmdf=gen_inst.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
trap "rm $cmdf" EXIT
./$cmdf


