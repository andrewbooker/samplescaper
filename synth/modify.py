#!/usr/bin/env python

import soundfile as sf
import sys
import os
import random
import math
import time

def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: ("wav" in f) and ("si" in f), os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])
    
def stretch():
    return 0.05 + (0.1 * (random.random() - 0.5))
    
class Linear():
    def __init__(self):
        self.start = 1.0
        self.end = 2.0 if random.random() > 0.8 else 0.5
        self.gradient = 0
        self.bufferStart = 0.1 + (0.3 * random.random())
        self.bufferEnd = 0.1 + (0.3 * random.random())
        self.startAt = 0

    def over(self, dataLength):
        self.startAt = dataLength * self.bufferStart
        dl = dataLength * (1 - (self.bufferStart + self.bufferEnd))
        self.gradient = (self.end - self.start) / dl
        return self

    def at(self, i):
        if i < self.startAt:
            return self.start
        v = self.start + ((i - self.startAt) * self.gradient)
        if (self.end > self.start and v > self.end) or (self.end < self.start and v < self.end):
           return self.end
        return v

    def describe(self):
        return "lin_%3f" % (self.start + self.gradient)

class Sine():
    def __init__(self):
        self.f = random.random() / 1102.5
        self.offset = 2 * random.random() * math.pi
        self.amplitude = 0.008 * random.random()

    def at(self, i):
        return 1.0 + (self.amplitude * math.sin(self.offset + (i * self.f)))

    def describe(self):
        return "sin_%3f" % self.f

class Effect():
    def __init__(self):
        self.description = ""

    def describe(self):
        return self.description

class Pitch(Effect):
    def appliedTo(self, data, sampleRate):
        out = []
        dataLength = len(data)
        eff = Linear().over(dataLength)

        self.description = eff.describe()
        done = False
        i = 0
        p = 0
        while not done:
            p += eff.at(i)
            p0 = math.floor(p)
            if p0 >= dataLength or i > (10 * sampleRate):
                done = True
            else:
                p1 = math.ceil(p)
                if p1 >= dataLength:
                    p1 = p0
                e = p - p0
                out.append(((1.0 - e) * data[p0]) + (e * data[p1]))
            i += 1

        return out

class Multiply(Effect):
    def __init__(self, fn):
        self.other = []
        self.otherFn = ""

        poolDir = os.path.dirname(fn)
        spl = os.path.basename(fn).split("_")
        note = None
        if len(spl) > 1 and spl[0].isnumeric():
            note = int(spl[0])
        files = [f for f in filter(lambda f: f not in fn and (f.startswith(str(note)) or f.startswith(str(note + 12)) or f.startswith(str(note - 12))), os.listdir(poolDir))]

        if len(files) > 0:
            self.otherFn = files[random.randint(0, len(files) - 1)]
            self.other = sf.read(os.path.join(poolDir, self.otherFn))[0]
            print("multiplying with", self.otherFn)

    def appliedTo(self, data, sampleRate):
        out = []
        o = len(self.other)
        self.description = "multiply"
        ld = min(len(data), len(self.other)) if o > 0 else len(data)
        for d in range(ld):
            m = abs(self.other[d]) if o > 0 else data[d]
            out.append(data[d] * m)
        return out

inDir = sys.argv[1]
outDir = sys.argv[2]
xFade = 441

while True:
    f = nextAudioFileFrom(inDir)
    if f is not None:
        print("using", os.path.basename(f))
        data, sampleRate = sf.read(f)

        effect = Pitch()
        out = effect.appliedTo(data, sampleRate)

        g = 1.0 / xFade
        l = len(out)
        sample = out[xFade:-xFade] + [(out[-xFade:][s] * (1.0 - (g * s))) + (out[s] * g * s) for s in range(0, xFade)]
        fn = "%s_%s.wav" % (os.path.basename(f).split(".")[0], effect.describe())
        sf.write(os.path.join(outDir, fn), sample, sampleRate)
        print("written", fn)

    if len(os.listdir(inDir)) > 30:
        print("dropping", f)
        os.remove(f)

    time.sleep(10)

