#!/usr/bin/env python

import math
import soundfile as sf
import random
import sys
import os

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

def convert(f, inDir, outDir):
    data, sampleRate = sf.read(os.path.join(inDir, f))
    dataLen = len(data)

    env = Envelope(sampleRate)
    pan = Pan(sampleRate)
    print("creating %.2fs" % env.lengthSecs, "file of", env.required, "samples")

    wave = []
    i = 0
    while i != env.required:
        s = env.vol(i) * data[i % dataLen]
        p = pan.at(i)
        wave.append([s * p, s * (1.0 - p)])
        i += 1

    sf.write(os.path.join(outDir, "looped_%s" % f), wave, sampleRate)


import time
inDir = sys.argv[1]
outDir = sys.argv[2]

while True:
    rawFiles = os.listdir(inDir)
    loopedFiles = [f[7:] for f in os.listdir(outDir)]

    for f in rawFiles:
        if f not in loopedFiles:
            convert(f, inDir, outDir)
        else:
            print("already done", f)

    time.sleep(5)
