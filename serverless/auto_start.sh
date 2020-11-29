#!/bin/bash

rm -rf ~/Music/pool
rm -rf ~/Music/samples
mkdir -p ~/Music/archives
mkdir -p ~/Music/samples
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool 46 &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool 3 &
~/Documents/samplescaper/record/ambient.sh &
