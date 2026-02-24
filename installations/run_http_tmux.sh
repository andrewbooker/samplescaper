#!/bin/bash
set -e

die() {
    echo $1
    exit
}


args=( "$@" )
synths=( "${args[@]:1}" )
baseDir="~/Documents/samplescaper"
synthDir="$baseDir/synth/http"
playDir="$baseDir/singleunit/cpp"
device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')
[ -n "$device" ] || die "no randomatones audio device"

basePort=9960

ports=()
synthCmds=()
for s in ${synths[@]}; do
    ports+=($basePort)
    synthCmds+=("cd $synthDir/$s; echo '$s on $basePort'; ./run.sh $basePort")
    ((++basePort))
done


tmuxCmds=()
tmuxCmds+=("tmux new-session \"htop\"\;")
tmuxCmds+=("split-window -h \"${synthCmds[0]}\"\;")

for ((i=1; i < ${#synths[@]}; ++i)); do
    calc="100 * (1.0 / (${#synths[@]} - ($i - 1)))"
    pc=$(awk "BEGIN {print int(0.5 + ($calc))}")
    tmuxCmds+=("select-pane -t 1 \; split-window -v -l '$pc%' \"${synthCmds[i]}\"\;")
done

playSize=80

cmdPlay="cd $playDir; ./player 8 $device ${ports[@]} 2>/dev/null"
cmdAuto="sleep 10; watch -n30 './auto_start_stop 09:32 15:27'"
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '${playSize}%' \"$cmdPlay\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '12%' \"$cmdAuto\"\;")
tmuxCmds+=("select-pane -t 1 \;")
echo 'compiling player...'
cd "../singleunit/cpp"
g++ player.cpp -o player -l sndfile -l portaudio -l curl
cd ..
echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
