if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi

synths=( "$@" )
baseDir="~/Documents/samplescaper"
synthDir="$baseDir/synth/http"
playDir="$baseDir/singleunit/cpp"
device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')
basePort=9960

ports=()
synthCmds=()
for s in ${synths[@]}; do
    ports+=($basePort)
    synthCmds+=("cd $synthDir/$s; echo '$s on $basePort'; ./run.sh $basePort")
    ((++basePort))
done

cmdR0="htop"
tmuxCmds=()
tmuxCmds+=("tmux new-session \"${synthCmds[0]}\"\;")
tmuxCmds+=("split-window -h \"$cmdR0\"\;")

for ((i=1; i < ${#synths[@]}; ++i)); do
    calc="100 * (1.0 / (${#synths[@]} - ($i - 1)))"
    pc=$(awk "BEGIN {print int(0.5 + ($calc))}")
    tmuxCmds+=("select-pane -t 0 \; split-window -v -l '$pc%' \"${synthCmds[i]}\"\;")
done

recDir=~/Music/recording
mkdir -p $recDir
audioRecFn=${recDir}/randomatones_$(date +"%Y%m%d_%H%M%S").wav
cmdRecord="sleep 1; ffmpeg -f alsa -channels 2 -sample_rate 44100 -i loopout $audioRecFn"
cmdPlay="cd $playDir; ./player 6 $device ${ports[@]} 2>/dev/null"
tmuxCmds+=("select-pane -t ${#ports[@]} \; split-window -v -l '20%' \"$cmdRecord\"\;")
tmuxCmds+=("select-pane -t ${#ports[@]} \; split-window -v -l '70%' \"$cmdPlay\"\;")
echo 'compiling player...'
if [ $? == 1 ]; then
    exit
fi
cd cpp
g++ player.cpp -o player -l sndfile -l portaudio -l curl
cd ..
echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
