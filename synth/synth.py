#!/usr/bin/env python

import math
import soundfile as sf
import random
import datetime

def freq(n):
    return math.pow(2, (n - 69)/12.0) * 440

def anywhereBetween(v1, v2):
    return v1 + (math.pow(random.random(), 3) * (v2 - v1))

def genQuadrants():
    nodes = [1.0, 0.0, -1.0, 0.0]
    q = []
    r = 1.0
    for v in range(len(nodes) - 1):
        l = 0.5 * r * random.random()
        q.append((l, nodes[v]))
        r -= l

    q.append((r, nodes[-1]))
    return q

def genRandomTemplateFrom(quadrants):
    q = 0
    qp = 0.0
    val = 0.0

    template = []
    templateLength = 256

    for i in range(templateLength):
        quadrant = quadrants[q];

        val = anywhereBetween(val, quadrant[1])
        template.append(val)

        qp += 1.0 / templateLength
        if qp > quadrant[0]:
            val = quadrant[1] 
            q += 1
            qp = 0.0

    return template

def sineTemplate():
    template = []
    templateLength = 256

    for i in range(templateLength):
        template.append(math.sin(2 * math.pi * i / templateLength))

    return template


class WaveIterator():
    def __init__(self, template, f, vol, sampleRate):
        self.template = template
        self.length = len(template)
        self.stretch = f * self.length / sampleRate
        self.pos = 0
        self.vol = vol
    
    def next(self):
        ps = math.modf(self.pos * self.stretch)
        p = ps[0] + (int(ps[1]) % self.length)
        p0 = math.floor(p)
        p1 = (p0 + 1) if p0 < (self.length - 1) else 0
        v0 = self.template[p0]
        v1 = self.template[p1]

        self.pos += 1
        return self.vol * (((p - p0) * v1) + ((p0 + 1 - p) * v0))

class Loopable():
    def __init__(self, data):
        self.pos = 0
        self.l = int(len(data) / 2)
        self.partA = data[:self.l]
        self.partB = data[self.l:]

    def _merge(self, fromB):
        if self.pos >= len(self.partA):
            return self.partA[-1]
        p = 1.0 * self.pos / self.l
        v = (fromB * (1.0 - p)) + (p * self.partA[self.pos])
        self.pos += 1
        return v

    def create(self):
        return [self._merge(s) for s in self.partB]


def modulate(f):
    r = (1.0 - (2 * random.random()))
    return f * (1 + (0.009 * r))

def withProbability(prob):
    return random.random() > (1.0 - prob)

def assembleWaves(f):
    waves = []
    t = sineTemplate() if withProbability(0.75) else genRandomTemplateFrom(genQuadrants())
    for i in range(random.randint(2, 6)):
        fr = f if i == 0 else modulate(f)
        waves.append(WaveIterator(t, fr, 0.6 + (0.4 * random.random()), 44100))
    return waves

MAX_LIVE_POOL_SIZE = 63
class Builder():
    def __init__(self, outDir):
        self.outDir = outDir
        files = [os.path.join(self.outDir, f) for f in os.listdir(self.outDir)]
        self.done = sorted(files, key=lambda f: os.path.getmtime(f))
        print(len(self.done), "files in pool already")

    def build(self, n):
        fn = "%d_%s.wav" % (n, datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S"))
        waves = assembleWaves(freq(n))
        durSecs = 2 + (5 * random.random())

        sampleRate = 44100
        templateLength = 256

        denominator = len(waves)
        data = []

        for i in range(int(sampleRate * durSecs)):
            v = 0.0
            for w in waves:
                v += w.next()

            data.append(v / denominator)

        print("writing", fn)
        fqfn = os.path.join(self.outDir, fn)
        sf.write(fqfn, Loopable(data).create(), sampleRate)
        self.done.append(fqfn)

        if len(self.done) > MAX_LIVE_POOL_SIZE:
            d = self.done[0]
            print("dropping", d)
            os.remove(d)
            self.done.remove(d)

import sys
import os
import time

outDir = sys.argv[1]
notes = [48, 50, 51, 53, 55, 56, 58]
builder = Builder(outDir)

i = 0
while True:
    n = notes[i]
    builder.build(n)
    builder.build(n + 12)
    builder.build(n + 24)
    i += 1
    if i == len(notes):
        i = 0

    time.sleep(30)

