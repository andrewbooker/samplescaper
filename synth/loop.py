#!/usr/bin/env python

import math
import soundfile as sf
import random
import sys
import os
#list files in the outdir
#if any file names from the indir are not in the outdir, generate


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

inFile = sys.argv[1]
outDir = sys.argv[2]

data, sampleRate = sf.read(inFile)
dataLen = len(data)

env = Envelope(sampleRate)
print("creating %.2fs" % env.lengthSecs, "file of", env.required, "samples")

wave = []
i = 0
while i != env.required:
    wave.append(env.vol(i) * data[i % dataLen])
    i += 1

sf.write(os.path.join(outDir, "test.wav"), wave, sampleRate)


