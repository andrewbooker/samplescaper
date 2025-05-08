baseDir="~/Documents/samplescaper"
synthDir="$baseDir/synth/http"
playDir="$baseDir/singleunit/cpp"

device=$(~/Documents/samplescaper/singleunit/play.py | sed -nE 's/\s*([0-9]+) randomatones, ALSA.*/\1/p')
cmdR0="htop"
cmdR1="cd $playDir; ./run_player.sh 6 $device 9966 9967 9968 9969 2>/dev/null"
cmdL0="cd $synthDir/cpp; echo 'c++'; ./run.sh 9966"
cmdL1="cd $synthDir/rust; echo 'rust'; ./run.sh 9967"
cmdL2="cd $synthDir/php; echo 'php'; ./run.sh 9968"
cmdL3="cd $synthDir/go; echo 'go'; ./run.sh 9969"

tmux new-session "$cmdL0"\; \
split-window -h "$cmdR0"\; \
select-pane -t 0 \; \
split-window -v "$cmdL2"\; \
select-pane -t 0 \; \
split-window -v "$cmdL1"\; \
select-pane -t 2 \; \
split-window -v "$cmdL3"\; \
select-pane -t 4 \; \
split-window -v -l '80%' "$cmdR1"\;
