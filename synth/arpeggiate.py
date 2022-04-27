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

def anyOf(a):
    return a[random.randint(0, len(a) - 1)]


inDir = sys.argv[1]
rawDir = os.path.join(inDir, "raw")
factoryDir = os.path.join(inDir, "factory")
outDir = os.path.join(inDir, "raw")
sampleRate = 44100

def run():
    allFiles = [f for f in filter(lambda fn: "arpeggiated" not in fn, os.listdir(rawDir))]
    fIdx = len([f for f in filter(lambda fn: "arpeggiated" in fn, os.listdir(outDir))])
    random.shuffle(allFiles)

    numToUse = anyOf([5, 7, 9, 10, 11, 13, 14])
    if len(allFiles) < numToUse:
        print("insufficient files to arpeggiate", numToUse)
        return

    toUseByNote = [(int(f.split("_")[0]), f) for f in allFiles[:numToUse]]
    print("using", len(toUseByNote), "files")
    toUseByNote.sort(key=lambda f: f[0])
    sortedFiles = [f[1] for f in toUseByNote]
    up = sortedFiles[::2]
    down = sortedFiles[1::2]
    down.reverse()

    toUse = up + down

    inf = [sf.read(os.path.join(rawDir, f))[0] for f in toUse]
    inFiles = [[len(f), f, 0] for f in inf]

    unitSampleLength = int(sampleRate * (0.05 + (0.1 * random.random())))
    xfadeLength = int(0.01 + (0.4 * random.random()) * unitSampleLength)
    print("unitSampleLength", unitSampleLength)
    pulses = random.randint(18, 50)
    print(pulses, "pulses")

    fqfn = os.path.join(factoryDir, "arpeggiated_%04d.wav" % fIdx)
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
    shutil.move(fqfn, outDir)
    print("moved to live pool")

while True:
    run()
    time.sleep(5)
