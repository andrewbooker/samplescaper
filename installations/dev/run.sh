#!/bin/bash
set -e

device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) front, ALSA.*/\1/p')
[ -n "$device" ] || die "no front audio device"
basePort=9960
base="/home/$USER/Documents/samplescaper"
synth="cd $base/synth/http/ada; ./run.sh $((basePort + 1))"
player="cd $base/singleunit/cpp; ./run_player.sh 2 5 $basePort"
multiplexer="cd $base/singleunit/cpp/multiplexer; ./run.sh $basePort"

tmuxCmds=("tmux")
tmuxCmds+=("new-session \"bash\"\;")
tmuxCmds+=("split-window -h \"$player\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' \"$synth\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' \"$multiplexer\"\;")
tmuxCmds+=("select-pane -t 2")

echo "${tmuxCmds[@]}" > _gen_tmux.sh
chmod +x _gen_tmux.sh
./_gen_tmux.sh
rm _gen_tmux.sh

