baseDir="~/Documents/samplescaper"
synthDir="$baseDir/synth/http"
playDir="$baseDir/singleunit/cpp"
device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')

basePort=9960
synths=("cpp" "rust" "go" "php" "py")
ports=()
synthCmds=()

for s in ${synths[@]}; do
    ports+=($basePort)
    synthCmds+=("cd $synthDir/$s; echo '$s'; ./run.sh $basePort")
    ((++basePort))
done

cmdR0="htop"
cmdR1="cd $playDir; ./run_player.sh 6 $device ${ports[@]} 2>/dev/null"
sh=$((100 / ${#ports[@]}))

tmuxCmds=()
tmuxCmds+=("tmux new-session \"${synthCmds[0]}\"\;")
tmuxCmds+=("split-window -h \"$cmdR0\"\;")

for ((i=1; i < ${#synthCmds[@]}; ++i)); do
    pc=$((100 * sh / (100 - ((i - 1) * sh))))
    tmuxCmds+=("select-pane -t 0 \; split-window -v -l '$pc%' \"${synthCmds[i]}\"\;")
done

tmuxCmds+=("select-pane -t ${#ports[@]} \; split-window -v -l '80%' \"$cmdR1\"")

echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
