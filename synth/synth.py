#!/usr/bin/env python

import math
import soundfile as sf
import random

def freq(n):
    return math.pow(2, (n - 69)/12.0) * 440

def anywhereBetween(v1, v2):
    return v1 + (random.random() * random.random() * random.random() * (v2 - v1))

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


class WaveIterator():

    def __init__(self, template, f, sampleRate):
        self.template = template
        self.length = len(template)
        self.stretch = f * self.length / sampleRate
        self.pos = 0
    
    def next(self):
        ps = math.modf(self.pos * self.stretch)
        p = ps[0] + (int(ps[1]) % self.length)
        p0 = math.floor(p)
        p1 = (p0 + 1) % self.length
        v0 = self.template[p0]
        v1 = self.template[p1]

        self.pos += 1
        return ((p - p0) * v1) + ((p1 - p) * v0)


class Loopable():
    def __init__(self, data):
        self.pos = 0
        self.l = int(len(data) / 2)
        self.partA = data[:self.l]
        self.partB = data[self.l:]

    def _merge(self, fromB):
        if self.pos >= len(self.partA):
            return fromB
        p = 1.0 * self.pos / self.l
        v = (fromB * (1.0 - p)) + (p * self.partA[self.pos])
        self.pos += 1
        return v

    def create(self):
        return [self._merge(s) for s in self.partB]


def modulate(f):
    r = (1.0 - (2 * random.random()))
    return f * (1 + (0.0095 * r))

def assembleWaves(f):
    waves = []
    for i in range(random.randint(2, 6)):
        fr = f if i == 0 else modulate(f)
        waves.append(WaveIterator(genRandomTemplateFrom(genQuadrants()), fr, 44100))
    return waves

def build(n):
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
    sf.write(os.path.join(outDir, fn), Loopable(data).create(), sampleRate)

import sys
import os
import time
import datetime
import time
outDir = sys.argv[1]
notes = [48, 50, 51, 53, 55, 56, 58]

for i in range(20):
    n = notes[random.randint(0, len(notes) - 1)]

    build(n)
    build(n + 12)
    build(n + 24)
    time.sleep(20)

