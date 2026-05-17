#!/bin/bash
set -e
base=$(dirname "$0")
motorServer="cd motors; ./server.py"
player="cd cpp; ./run_player.sh 2 8 192.168.1.88:9964"

tmuxCmds=("tmux")
tmuxCmds+=("new-session \"$motorServer\"\;")
tmuxCmds+=("split-window -h \"$player\"\;")
tmuxCmds+=("select-pane -t 2")

echo "${tmuxCmds[@]}" > _gen_tmux.sh
chmod +x _gen_tmux.sh
./_gen_tmux.sh
rm _gen_tmux.sh

