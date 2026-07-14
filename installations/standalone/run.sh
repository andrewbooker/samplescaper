#!/bin/bash
set -e
if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi

base="/home/$USER/Documents/samplescaper"
device=$($base/singleunit/play.py | sed -nE 's/[\*|<]\s*([0-9]+) default, ALSA.*/\1/p')
[ -n "$device" ] || die "default audio device not available"
basePort=9960
synths=(haskell go ada pascal)

echo 'checking synths are compiled'
for synth in ${synths[@]}; do
    echo "compiling $synth"
    cd $base/synth/http/$synth
    if [ ! -f server ]; then
        ./compile.sh
    fi
    cd - >/dev/null
done
echo 'compiling player'
cd $base/singleunit/cpp/player
[ -f ./player ] || ./compile.sh
cd - >/dev/null
echo 'compiling multiplexer'
cd $base/singleunit/cpp/multiplexer
[ -f ./server ] || ./compile.sh
cd - >/dev/null

player="cd $base/singleunit/cpp; ./run_player.sh 6 $device $basePort"
multiplexer="cd $base/singleunit/cpp/multiplexer; ./run.sh $basePort"

tmuxCmds=("tmux")
tmuxCmds+=("new-session \"htop\"\;")
tmuxCmds+=("split-window -h \"bash\"\;")

for ((i=1; i < ${#synths[@]}; ++i)); do
    calc="100 * (1.0 / (${#synths[@]} - ($i - 1)))"
    pc=$(awk "BEGIN {print int(0.5 + ($calc))}")
    tmuxCmds+=("select-pane -t 1 \; split-window -v -l '$pc%' \"bash\"\;")
done

tmuxCmds+=("select-pane -t 0 \; split-window -v -l '85%' \"bash\"\;") #player
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '50%' \"bash\"\;") #multiplexer
tmuxCmds+=("select-pane -t 2 \; split-window -v -l '50%' \"bash\"\;") #recorder

rightPaneStart=$((${#tmuxCmds[@]} - ${#synths[@]} - 1))
synthPane=0
for synth in ${synths[@]}; do
    t=$((2 + synthPane))
    c="cd $base/synth/http/$synth; sleep $t; ./run.sh $((basePort + synthPane + 1))"
    p=$((rightPaneStart + synthPane))
    tmuxCmds+=("send-keys -t $p \"$c\" ENTER \;")
    ((synthPane+=1))
done

tmuxCmds+=("send-keys -t 1 \"$player\" ENTER \;")
tmuxCmds+=("send-keys -t 2 \"$multiplexer\" ENTER \;")
tmuxCmds+=("select-pane -t 3")

script=_gen_tmux.sh
trap "rm $script" EXIT
echo "${tmuxCmds[@]}" > $script
chmod +x $script
./$script

