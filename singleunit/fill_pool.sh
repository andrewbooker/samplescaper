mkdir -p ~/Music/archives
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped
mkdir -p ~/Documents/logs

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool ~/Documents/samplescaper/key.json ~/Documents/samplescaper/config.json &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool mono
