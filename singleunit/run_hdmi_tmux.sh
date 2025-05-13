if [[ "$(aplay -l | grep Loopback)" != *'Loopback'* ]]; then
    echo 'setting up loopback (requires sudo)'
    sudo modprobe snd-aloop
fi

cmdDir="~/Documents/samplescaper/singleunit"
logDir="/var/log/samplescaper"

echo '' > $logDir/player.log
echo '' > $logDir/synth.log
echo '' > $logDir/looper.log
rm  ~/Music/pool/raw/*.wav
rm  ~/Music/pool/factory/*.wav
rm  ~/Music/pool/looped/*.wav
recording=~/Music/randomatones_$(date +"%Y%m%d_%H%M%S").wav
cmdR2="$cmdDir/fill_pool.sh & $cmdDir/run.sh"
cmdR0="htop"
cmdR1="cd ~; exec bash"
cmdR3="tail -f $logDir/player.log"
cmdL0="tail -f $logDir/synth.log"
cmdL1="tail -f $logDir/looper.log"
cmdL2="sleep 3; ffmpeg -f alsa -channels 2 -sample_rate 44100 -i loopout -y $recording"

tmux new-session "$cmdL0"\; \
split-window -h "$cmdR0"\; \
select-pane -t 0 \; \
split-window -v -l '20%' "$cmdL2"\; \
select-pane -t 0 \; \
split-window -v -l '50%' "$cmdL1"\; \
select-pane -t 3 \; \
split-window -v "$cmdR3"\; \
select-pane -t 3 \; \
split-window -v "$cmdR1"\; \
select-pane -t 4 \; \
split-window -v "$cmdR2"\; \
select-pane -t 4
