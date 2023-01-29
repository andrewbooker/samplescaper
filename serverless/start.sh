#!/bin/bash

arpeggiate=$(jq .arpeggiate ~/Documents/samplescaper/config.json)
sweepRaw=$(jq .sweepRaw ~/Documents/samplescaper/config.json)
sweepLooped=$(jq .sweepLooped ~/Documents/samplescaper/config.json)

rm -rf ~/Music/pool
rm -rf ~/Music/modified*
mkdir -p ~/Music/archives
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

if [ $sweepRaw = 'true' ]
then
    mkdir -p ~/Music/modifiedRaw
    python ~/Documents/samplescaper/synth/modify.py ~/Music/modifiedRaw ~/Music/pool/raw ~/Documents/samplescaper/config.json &
    python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/raw ~/Music/modifiedRaw 37.2 &
fi
if [ $sweepLooped = 'true' ]
then
    mkdir -p ~/Music/modifiedLooped
    python ~/Documents/samplescaper/synth/modify.py ~/Music/modifiedLooped ~/Music/pool/looped ~/Documents/samplescaper/config.json &
    python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/looped ~/Music/modifiedLooped 21.4 &
fi

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool ~/Documents/samplescaper/config.json &
if [ "$arpeggiate" != 'null' ]
then
    python ~/Documents/samplescaper/synth/arpeggiate.py ~/Music/pool ~/Documents/samplescaper/config.json &
fi
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool ~/Documents/samplescaper/config.json $(jq .leftRelativeToRight ~/Documents/static.json) $(jq -r .audioDevice ~/Documents/static.json) &
cd ~/Documents/samplescaper/serverless/handset
./serve.sh &

