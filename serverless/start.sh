#!/bin/bash

rm -rf ~/Music/pool
rm -rf ~/Music/samples*
mkdir -p ~/Music/archives
#mkdir -p ~/Music/samples1
#mkdir -p ~/Music/samples2
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

#python ~/Documents/samplescaper/synth/modify.py ~/Music/samples1 ~/Music/pool/looped ~/Documents/samplescaper/config.json &
#python ~/Documents/samplescaper/synth/modify.py ~/Music/samples2 ~/Music/pool/raw ~/Documents/samplescaper/config.json &
#python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/looped ~/Music/samples1 21.4 &
#python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/raw ~/Music/samples2 37.2 &
python ~/Documents/samplescaper/synth/synth.py ~/Music/pool ~/Documents/samplescaper/config.json &
python ~/Documents/samplescaper/synth/arpeggiate.py ~/Music/pool &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool ~/Documents/samplescaper/config.json &
cd ~/Documents/samplescaper/serverless/handset
./serve.sh &

