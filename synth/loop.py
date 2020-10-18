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
    def __init__(self, sampleRate):
        self.freqHz = 0.5 * (5 * random.random())
        self.radPerSample = self.freqHz * 2 * math.pi / sampleRate
        self.offset = random.random() * sampleRate / self.freqHz

    def at(self, i):
        return 0.5 * (1.0 + math.sin((i + self.offset) * self.radPerSample))


def pannedSample(val, pan):
    return [val * pan, val * (1.0 - pan)]

def convert(f, inDir, factoryDir, outDir):
    data, sampleRate = sf.read(os.path.join(inDir, f))
    dataLen = len(data)

    env = Envelope(sampleRate)
    pan = Pan(sampleRate)
    start = time.monotonic()
    print("creating %.2fs" % env.lengthSecs, "file of", env.required, "samples")

    fqfn = os.path.join(factoryDir, "looped_%s" % f)
    sf.write(fqfn, [pannedSample(env.vol(i) * data[i % dataLen], pan.at(i)) for i in range(0, env.required)], sampleRate)
    print("moving to live pool after %.2fs" % (time.monotonic() - start))
    shutil.move(fqfn, outDir)


inDir = os.path.join(sys.argv[1], "raw")
factoryDir = os.path.join(sys.argv[1], "factory")
outDir = os.path.join(sys.argv[1], "looped")

while True:
    rawFiles = os.listdir(inDir)
    loopedFiles = [f[7:] for f in os.listdir(outDir)]

    for f in rawFiles:
        if f not in loopedFiles:
            try:
                convert(f, inDir, factoryDir, outDir)
            except:
                print("failed to create loop for", f, "probably still being written")
        else:
            print("already done", f)

    time.sleep(5)
