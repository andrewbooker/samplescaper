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
    return 0.05 + (0.1 * random.random())
    
class Linear():
    def __init__(self, dataLength):
        self.intercept = stretch()
        self.gradient = (stretch() - self.intercept) / dataLength

    def at(self, i):
        return self.intercept + (i * self.gradient)

    def describe(self):
        return "lin_%3f" % (self.intercept + self.gradient)

class Sine():
    def __init__(self):
        self.f = random.random() / 1102.5
        self.offset = 2 * random.random() * math.pi
        self.amplitude = 0.025 * random.random()

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

        lin = Linear(dataLength)
        sin = Sine()
        self.description = "%s_%s" % (lin.describe(), sin.describe())
        done = False
        i = 0
        while not done:
            p = i / (lin.at(i) + sin.at(i))
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
    def appliedTo(self, data, sampleRate):
        out = []
        self.description = "multiply"
        for d in data:
            out.append(d * abs(d))
        return out

inDir = sys.argv[1]
outDir = sys.argv[2]
xFade = 441

while True:
    f = nextAudioFileFrom(inDir)
    if f is not None:
        print("using", os.path.basename(f))
        data, sampleRate = sf.read(f)

        effect = Multiply()
        out = effect.appliedTo(data, sampleRate)

        g = 1.0 / xFade
        l = len(out)
        sample = out[xFade:-xFade] + [(out[-xFade:][s] * (1.0 - (g * s))) + (out[s] * g * s) for s in range(0, xFade)]
        fn = "%s_%s.wav" % (os.path.basename(f).split(".")[0], effect.describe())
        print("writing", fn)
        sf.write(os.path.join(outDir, fn), sample, sampleRate)

    if len(os.listdir(inDir)) > 30:
        print("dropping", f)
        os.remove(f)

    time.sleep(10)

