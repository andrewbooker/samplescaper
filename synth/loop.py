#!/usr/bin/env python

import math
import soundfile as sf
import random
import sys
import os
import shutil
import time

class Envelope():
    def __init__(self, sampleRate):
        upSecs = 0.5 + (2 * random.random())
        downSecs = 5.0 + (5 * random.random())
        self.lengthSecs = upSecs + downSecs + (random.random() * 5.0)
        self.up = int(upSecs * sampleRate)
        self.downStart = int((self.lengthSecs - downSecs) * sampleRate)
        self.required = int(self.lengthSecs * sampleRate)
        self.down = self.required - self.downStart
        
    def vol(self, i):
        if i < self.up:
            return 1.0 * i / self.up
        if i > self.downStart:
            return 1.0 - (float(i - self.downStart) / self.down)
        return 1.0

class Pan():
    def __init__(self, sampleRate, numberOfFiles):
        self.freqHz = 0.5 * (numberOfFiles * 5 * random.random())
        self.radPerSample = self.freqHz * 2 * math.pi / sampleRate
        self.offset = random.random() * sampleRate / self.freqHz

    def at(self, i):
        return 0.5 * (1.0 + math.sin((i + self.offset) * self.radPerSample))


def pannedSample(vals, pan):
    return [vals[0] * pan, vals[len(vals) - 1] * (1.0 - pan)]

def convert(files, inDir, factoryDir, outDir):
    fd = [sf.read(os.path.join(inDir, f))[0] for f in files]
    fileData = [(len(d), d) for d in fd]

    sampleRate = 44100
    env = Envelope(sampleRate)
    pan = Pan(sampleRate, len(files))
    start = time.monotonic()
    print("creating %.2fs" % env.lengthSecs, "file of", env.required, "samples")

    fqfn = os.path.join(factoryDir, "looped_%s" % "__".join(files))
    sf.write(fqfn, [pannedSample([env.vol(i) * f[1][i % f[0]] for f in fileData], pan.at(i)) for i in range(0, env.required)], sampleRate)
    print("moving to live pool after %.2fs" % (time.monotonic() - start))
    shutil.move(fqfn, outDir)


inDir = os.path.join(sys.argv[1], "raw")
factoryDir = os.path.join(sys.argv[1], "factory")
outDir = os.path.join(sys.argv[1], "looped")

def convertItems(rawFiles, outDir):
    loopedFiles = [f[7:] for f in os.listdir(outDir)]
    takeFirstTwo = (random.random() > 0.3) and (len(rawFiles) > 1)

    if takeFirstTwo:
        convert([f for f in rawFiles][:2], inDir, factoryDir, outDir)
        return

    for f in rawFiles:
        if f not in loopedFiles:
            try:
                convert([f], inDir, factoryDir, outDir)
            except:
                print("failed to create loop for", f, "probably still being written")
        else:
            print("already done", f)
    time.sleep(5)

while True:
    rawFiles = os.listdir(inDir)
    random.shuffle(rawFiles)
    convertItems(rawFiles, outDir)

