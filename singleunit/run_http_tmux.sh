synths=("cpp" "py" "php" "perl" "rust" "go")

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
cmdR1="cd $playDir; ./run_player.sh 6 $device ${ports[@]} 2>/dev/null"
tmuxCmds=()
tmuxCmds+=("tmux new-session \"${synthCmds[0]}\"\;")
tmuxCmds+=("split-window -h \"$cmdR0\"\;")

for ((i=1; i < ${#synths[@]}; ++i)); do
    calc="100 * (1.0 / (${#synths[@]} - ($i - 1)))"
    pc=$(awk "BEGIN {print int(0.5 + ($calc))}")
    tmuxCmds+=("select-pane -t 0 \; split-window -v -l '$pc%' \"${synthCmds[i]}\"\;")
done

tmuxCmds+=("select-pane -t ${#ports[@]} \; split-window -v -l '80%' \"$cmdR1\"")

echo "${tmuxCmds[@]}" > _gen.sh
chmod +x _gen.sh
./_gen.sh
rm _gen.sh
