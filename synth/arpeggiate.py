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
fIdx = len(os.listdir(outDir))
random.shuffle(allFiles)

toUse = allFiles[:random.randint(3, 11)]

inf = [sf.read(os.path.join(rawDir, f))[0] for f in toUse]
inFiles = [[len(f), f, 0] for f in inf]

sampleRate = 44100

start = time.monotonic()
unitSampleLength = int(sampleRate * (0.05 + (0.1 * random.random())))
print("unitSampleLength", unitSampleLength)
pulses = 20


fqfn = os.path.join(factoryDir, "arpeggiated_%d.wav" % fIdx)
outfile = sf.SoundFile(fqfn, "w", samplerate=sampleRate, channels=1)

print("writing", fqfn)
for p in range(pulses):
    for fd in inFiles:
        outfile.write([fd[1][s] for s in range(fd[2], fd[2] + unitSampleLength)])
        fd[2] += unitSampleLength
        if (fd[2] + unitSampleLength) > fd[0]:
            fd[2] = 0

outfile.close()
print("moving to live pool after %.2fs" % (time.monotonic() - start))
shutil.move(fqfn, outDir)
