#!/bin/bash
set -e
base=$(dirname "$0")
motorServer="cd motors; ./server.py"
synth="cd ../synth/http/ada; ./run.sh 9964"
player="cd cpp; ./run_player.sh 2 8"

tmuxCmds=("tmux")
tmuxCmds+=("new-session \"$motorServer\"\;")
tmuxCmds+=("split-window -h \"$player\"\;")
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '50%' \"$synth\"\;")
tmuxCmds+=("select-pane -t 2")

echo "${tmuxCmds[@]}" > _gen_tmux.sh
chmod +x _gen_tmux.sh
./_gen_tmux.sh
rm _gen_tmux.sh

