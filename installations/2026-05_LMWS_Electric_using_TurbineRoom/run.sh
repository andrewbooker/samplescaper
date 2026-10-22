#!/bin/bash
set -e

die() {
    echo $1
    exit
}

here=$(pwd)
synths=(rust go pascal)
baseDir="/home/$USER/Documents/samplescaper"
synthDir="$baseDir/synth/http"
playDir="$baseDir/singleunit/cpp/player"
muxDir="$baseDir/singleunit/cpp/multiplexer"
device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')
[ -n "$device" ] || die "no randomatones audio device"

basePort=9960
synthPort=$basePort
delay=5
synthCmds=()
for s in ${synths[@]}; do
    ((++synthPort))
    synthCmds+=("cd $synthDir/$s; echo '$s on $synthPort'; sleep $delay; ./run.sh $synthPort")
    ((++delay))
done


tmuxCmds=()
tmuxCmds+=("tmux new-session \"htop\"\;")
tmuxCmds+=("split-window -h \"${synthCmds[0]}\"\;")

for ((i=1; i < ${#synths[@]}; ++i)); do
    calc="100 * (1.0 / (${#synths[@]} - ($i - 1)))"
    pc=$(awk "BEGIN {print int(0.5 + ($calc))}")
    tmuxCmds+=("select-pane -t 1 \; split-window -v -l '$pc%' \"${synthCmds[i]}\"\;")
done

converterPort=$((synthPort+1))
captureLoc=/home/$USER/Music/pool/live
mkdir -p $captureLoc
converterCmd+="cd $baseDir/distributed/cpp; ./run.sh $converterPort $captureLoc"
fetchCmd+="cd $here; watch -n10 './fetch.sh $captureLoc'"
fileListCmd+="sleep 13; curl http://localhost:$basePort?add=$converterPort"
#tmuxCmds+=("select-pane -t 1 \; split-window -v -l '30%' \"$converterCmd\"\;")
#tmuxCmds+=("select-pane -t 2 \; split-window -v -l '50%' \"$fileListCmd\"\;")

playSize=85

cmdPlay="cd $playDir; ./player 8 $device $basePort 2>/dev/null"
cmdMux="cd $muxDir; ./run.sh $basePort"
cmdAuto="cd $baseDir/singleunit; sleep 10; watch -n30 './auto_start_stop 09:03 14:57 1 2 3'"
tmuxCmds+=("select-pane -t 0 \; split-window -v -l '${playSize}%' \"$cmdPlay\"\;")
tmuxCmds+=("select-pane -t 1 \; split-window -v -l '40%' \"$cmdMux\"\;")
tmuxCmds+=("select-pane -t 2 \; split-window -v -l '30%' \"$cmdAuto\"\;")
tmuxCmds+=("select-pane -t 1 \;")
echo 'compiling player...'
cd "$playDir"
g++ player.cpp -o player -l sndfile -l portaudio -l curl
cd ..

echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
