#!/bin/bash

rm -rf ~/Music/pool
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/playRaw.py ~/Music/pool &
