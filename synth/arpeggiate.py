#!/usr/bin/env python

import sys
import os
import random
import soundfile as sf
import time
import shutil


def outArr(xfBuff, buff):
    l = len(xfBuff)
    if l == 0:
        return buff

    xb = [((1.0 - (float(i) / l)) * xfBuff[i]) + (float(i) * buff[i] / l) for i in range(l)]
    return xb + buff[l:]


inDir = sys.argv[1]
rawDir = os.path.join(inDir, "raw")
factoryDir = os.path.join(inDir, "factory")
outDir = os.path.join(inDir, "raw")
allFiles = [f for f in filter(lambda fn: "arpeggiated" not in fn, os.listdir(rawDir))]
fIdx = len([f for f in filter(lambda fn: "arpeggiated" in fn, os.listdir(outDir))])
random.shuffle(allFiles)


toUseByNote = [(int(f.split("_")[0]), f) for f in allFiles[:random.randint(3, 11)]]
print("using", len(toUseByNote), "files")
toUseByNote.sort(key=lambda f: f[0])
sortedFiles = [f[1] for f in toUseByNote]
up = sortedFiles[::2]
down = sortedFiles[1::2]
down.reverse()

toUse = up + down

inf = [sf.read(os.path.join(rawDir, f))[0] for f in toUse]
inFiles = [[len(f), f, 0] for f in inf]

sampleRate = 44100

start = time.monotonic()
unitSampleLength = int(sampleRate * (0.05 + (0.1 * random.random())))
xfadeLength = int(0.01 + (0.4 * random.random()) * unitSampleLength)
print("unitSampleLength", unitSampleLength)
pulses = 20


fqfn = os.path.join(factoryDir, "arpeggiated_%d.wav" % fIdx)
outfile = sf.SoundFile(fqfn, "w", samplerate=sampleRate, channels=1)

print("writing", fqfn)
xfBuff = []
for p in range(pulses):
    for fd in inFiles:
        start = fd[2]
        end = start + unitSampleLength
        outfile.write(outArr(xfBuff, [fd[1][s] for s in range(start, end)]))
        xfBuff = [fd[1][s] for s in range(end, end + xfadeLength)]
        fd[2] += unitSampleLength
        if (fd[2] + unitSampleLength + xfadeLength) > fd[0]:
            fd[2] = 0

outfile.close()
print("moving to live pool after %.2fs" % (time.monotonic() - start))
shutil.move(fqfn, outDir)
