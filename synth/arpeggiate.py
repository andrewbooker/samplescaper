#!/usr/bin/env python

import sys
import os
import random
import soundfile as sf
import time
import shutil


inDir = sys.argv[1]
rawDir = os.path.join(inDir, "raw")
factoryDir = os.path.join(inDir, "factory")
outDir = os.path.join(inDir, "looped")
allFiles = os.listdir(rawDir)
random.shuffle(allFiles)

toUse = allFiles[:6]

inf = [sf.read(os.path.join(rawDir, f))[0] for f in toUse]
inFiles = [(len(f), f) for f in inf]

sampleRate = 44100

start = time.monotonic()
unitSampleLength = int(sampleRate * (0.05 + (0.1 * random.random())))
print("unitSampleLength", unitSampleLength)
pulses = 20

f = 4
fqfn = os.path.join(factoryDir, "arpeggiated_%d.wav" % f)
outfile = sf.SoundFile(fqfn, "w", samplerate=sampleRate, channels=1)

print("writing", fqfn)
for p in range(pulses):
    startIdx = p * unitSampleLength
    for fd in inFiles:
        outfile.write([fd[1][s] for s in range(startIdx, startIdx + unitSampleLength)])

outfile.close()
print("moving to live pool after %.2fs" % (time.monotonic() - start))
shutil.move(fqfn, outDir)
