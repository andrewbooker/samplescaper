#!/bin/bash

rm -rf ~/Music/pool
rm -rf ~/Music/samples*
mkdir -p ~/Music/archives
mkdir -p ~/Music/samples1
mkdir -p ~/Music/samples2
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

tonic=66

python ~/Documents/samplescaper/synth/modify.py ~/Music/samples1 ~/Music/pool/looped &
python ~/Documents/samplescaper/synth/modify.py ~/Music/samples2 ~/Music/pool/raw &
python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/looped ~/Music/samples1 10 &
python ~/Documents/samplescaper/synth/recycle.py ~/Music/pool/raw ~/Music/samples2 33 &
python ~/Documents/samplescaper/synth/synth.py ~/Music/pool $tonic &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool ~/Documents/samplescaper/serverless/config.json &

