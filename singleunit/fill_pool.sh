mkdir -p ~/Music/archives
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped
log_dir=/var/log/samplescaper

python -u ~/Documents/samplescaper/synth/synth.py ~/Music/pool ~/Documents/samplescaper/key.json ~/Documents/samplescaper/config.json > $log_dir/synth.log 2>&1 &
python -u ~/Documents/samplescaper/synth/loop.py ~/Music/pool mono > $log_dir/looper.log 2>&1
