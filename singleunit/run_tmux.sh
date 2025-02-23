cmdDir="~/Documents/samplescaper/singleunit"
logDir="/var/log/samplescaper"

cmdL0="$cmdDir/fill_pool.sh & $cmdDir/run.sh"
cmdR0="htop"
cmdR1="tail -f $logDir/player.log"
cmdL1="tail -f $logDir/synth.log"
cmdL2="tail -f $logDir/looper.log"

tmux new-session "$cmdL0"\; \
split-window -h "$cmdR0"\; \
select-pane -t 0 \; \
split-window -v "$cmdL2"\; \
select-pane -t 0 \; \
split-window -v "$cmdL1"\; \
select-pane -t 3 \; \
split-window -v "$cmdR1"\; \
select-pane -t 3 \; \
split-window "cd ~; exec bash"
