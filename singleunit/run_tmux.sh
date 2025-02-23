
cmdL0="./fill_pool.sh & ./run.sh"
cmdL1="tail -f /var/log/samplescaper/synth.log"
cmdL2="tail -f /var/log/samplescaper/looper.log"
cmdR0="htop"
cmdR1="tail -f /var/log/samplescaper/player.log"

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
