cmdPlay="./run.sh"
cmdFillPool="./fill_pool.sh"


cmdL0=$cmdFillPool
cmdL1=$cmdPlay
cmdL2="tail -f /var/log/samplescaper/synth.log"
cmdL3="tail -f /var/log/samplescaper/looper.log"
cmdR0="htop"
cmdR1="tail -f /var/log/samplescaper/player.log"

tmux new-session "$cmdL0"\; \
split-window -h "$cmdR0"\; \
select-pane -t 0 \; \
split-window -v "$cmdL2"\; \
split-window -v "$cmdL3"\; \
select-pane -t 0 \; \
split-window -v "$cmdL1"\; \
select-pane -t 4 \; \
split-window -v "$cmdR1"\; \
select-pane -t 4 \; \
split-window
