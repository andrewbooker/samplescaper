#!/bin/bash
f='samples.zip'
captureLoc=$1
cd $captureLoc
pwd
if [ -f $f ]; then
    rm *.zip
fi
curl http://chorusflow.com/randomatones/samples/$f --output $f
echo 'extracting samples'
rm *.wav
unzip $f
rm *.zip
ls -lth
cd -


