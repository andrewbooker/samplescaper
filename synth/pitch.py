#!/usr/bin/env python

import soundfile as sf
import sys
import os
import random
import math
import time

def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
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
        return "linear_%3f" % (self.intercept + self.gradient)

class Sine():
    def __init__(self):
        self.f = random.random() / 2205
        self.offset = 2 * random.random() * math.pi
        self.amplitude = 0.05 * random.random()

    def at(self, i):
        return 1.0 + (self.amplitude * math.sin(self.offset + (i * self.f)))

    def describe(self):
        return "sine_%3f" % self.f

inDir = sys.argv[1]
outDir = sys.argv[2]

while True:
    f = nextAudioFileFrom(inDir)
    if f is not None:
        print("using", os.path.basename(f))
        data, sampleRate = sf.read(f)

        out = []
        dataLength = len(data)
        
        lin = Linear(dataLength)
        sin = Sine()
        print(lin.describe(), sin.describe())
        pref = "linSine"
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

        sf.write(os.path.join(outDir, "%s_%s.wav" % (os.path.basename(f), pref)), out, sampleRate)
    time.sleep(10)

