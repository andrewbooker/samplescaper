#!/bin/bash

cnx=$(nc github.com 443 -zv 2>&1)
if [[ $cnx =~ "succeeded" ]]; then
    cd ~/Documents/samplescaper
    git pull
    cd
fi

rm -rf ~/Music/pool
mkdir -p ~/Music/pool/factory
mkdir -p ~/Music/pool/raw
mkdir -p ~/Music/pool/looped

python ~/Documents/samplescaper/synth/synth.py ~/Music/pool &
python ~/Documents/samplescaper/synth/loop.py ~/Music/pool &
python ~/Documents/samplescaper/serverless/controller.py ~/Music/pool &
