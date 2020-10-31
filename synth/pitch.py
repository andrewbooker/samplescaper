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
    s = 0.05 + (1.7 * random.random())
    if s > 0.7 and s < 1.3:
        return stretch()
    return s
    
class Linear():
    def __init__(self, dy, dx, c):
        self.gradient = dy / dx
        self.intercept = c
    
    def at(self, i):
        return self.intercept + (i * self.gradient)

inDir = sys.argv[1]
outDir = sys.argv[2]

while True:
    f = nextAudioFileFrom(inDir)
    print("using", os.path.basename(f))
    data, samplerate = sf.read(f)
    
    sd = len(data)
    out = []
    s1 = stretch()
    s2 = stretch()
    
    print(s1, "to", s2)
    sa = 0.5 * (s1 + s2)
    rng = int(sd * sa)
    fi = Linear(s2 - s1, rng, s1)
    for i in range(rng):
        p = i / fi.at(i)
        p0 = math.floor(p)
        if p0 >= sd:
            break
        p1 = math.ceil(p)
        if p1 >= sd:
            p1 = p0
        e = p - p0
        out.append(((1.0 - e) * data[p0]) + (e * data[p1]))

    sf.write(os.path.join(outDir, "%s_%.3f.wav" % (os.path.basename(f), sa)), out, samplerate)
    time.sleep(2)

