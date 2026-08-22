#!/bin/bash
set -e

device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) front, ALSA.*/\1/p')
[ -n "$device" ] || die "no front audio device"
basePort=9960
base="/home/$USER/Documents/samplescaper"
synth="cd $base/synth/http/ada; ./run.sh $((basePort + 1))"
player="cd $base/singleunit/cpp; ./run_player.sh 2 $device $basePort"
multiplexer="cd $base/singleunit/cpp/multiplexer; ./run.sh $basePort"

tmuxCmds=("tmux")
tmuxCmds+=("new-session \;")
tmuxCmds+=("split-window -h bash \;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' bash \;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' bash \;")

tmuxCmds+=("send-keys -t 0 htop ENTER \;")
tmuxCmds+=("send-keys -t 3 \"$player\" ENTER \;")
tmuxCmds+=("send-keys -t 1 \"$multiplexer\" ENTER \;")
tmuxCmds+=("send-keys -t 2 \"sleep 5; $synth\" ENTER \;")
tmuxCmds+=("select-pane -t 3")

tms='_gen_tmux.sh'
echo "${tmuxCmds[@]}" > $tms
trap "rm $tms" EXIT
chmod +x $tms
./$tms

