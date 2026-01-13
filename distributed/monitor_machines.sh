#!/bin/bash
set -e

localIp=192.168.1.88
remoteHanging="ssh pi@192.168.1.99"
remoteBlue="ssh pi@192.168.1.19"

tmuxCmds=(tmux)
tmuxCmds+=("new-session \"htop\"\;")
tmuxCmds+=("split-window -h \"$remoteHanging\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v \"$remoteBlue\"\;")

tmuxCmds+=("send-keys -t 1 \"htop\" ENTER \;")
tmuxCmds+=("send-keys -t 2 \"htop\" ENTER \;")

cmdf=_gen2.sh
echo "${tmuxCmds[@]}" > $cmdf
chmod +x $cmdf
./$cmdf
rm $cmdf
