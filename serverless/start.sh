#!/bin/bash

arpeggiate=$(jq .arpeggiate ~/Documents/samplescaper/config.json)
sweepRaw=$(jq -r .sweepRaw ~/Documents/samplescaper/config.json)
sweepLooped=$(jq -r .sweepLooped ~/Documents/samplescaper/config.json)

rm -rf ~/Music/pool
mkdir -p ~/Music/archives
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped
mkdir -p ~/Documents/logs

if [ $sweepRaw != 'null' ]
then
    mkdir -p ~/Music/pool/modifiedRaw
    python ~/Documents/samplescaper/synth/modify.py ~/Music/pool/modifiedRaw ~/Music/pool/raw ~/Documents/samplescaper/key.json $sweepRaw &
    python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/raw ~/Music/pool/modifiedRaw &
fi
if [ $sweepLooped != 'null' ]
then
    mkdir -p ~/Music/pool/modifiedLooped
    python ~/Documents/samplescaper/synth/modify.py ~/Music/pool/modifiedLooped ~/Music/pool/looped ~/Documents/samplescaper/key.json $sweepLooped &
    python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/looped ~/Music/pool/modifiedLooped &
fi

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool ~/Documents/samplescaper/key.json ~/Documents/samplescaper/config.json &
if [ "$arpeggiate" != 'null' ]
then
    python ~/Documents/samplescaper/synth/arpeggiate.py ~/Music/pool ~/Documents/samplescaper/config.json &
fi
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool ~/Documents/samplescaper $(jq .leftRelativeToRight ~/Documents/static.json) &
cd ~/Documents/samplescaper/serverless/handset
./serve.sh &

