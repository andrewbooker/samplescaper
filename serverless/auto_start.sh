#!/bin/bash

rm -rf ~/Music/pool
mkdir -p ~/Music/archives
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool &
