#!/usr/bin/env python

import math
import soundfile as sf
import random
import datetime
import shutil
import json
import os
import sys

TEMPLATE_LENGTH = 256
SAMPLE_RATE = 44100

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

def randomTemplateFrom(quadrants):
    q = 0
    qp = 0.0
    val = 0.0

    template = []

    for i in range(TEMPLATE_LENGTH):
        quadrant = quadrants[q];

        val = anywhereBetween(val, quadrant[1])
        template.append(val)

        qp += 1.0 / TEMPLATE_LENGTH
        if qp > quadrant[0]:
            val = quadrant[1] 
            q += 1
            qp = 0.0

    return template

class PureSineTemplate():
    def __init__(self):
        self.a = []

        for i in range(TEMPLATE_LENGTH):
            self.a.append(math.sin(2 * math.pi * i / TEMPLATE_LENGTH))

    def get(self):
        return self.a;

    def step(self):
        return None

def squareTemplate(quadrants):
    q = 0
    qp = 0.0
    template = []

    for i in range(TEMPLATE_LENGTH):
        if q < 2:
            template.append(0.6)
        else:
            template.append(-0.6)

        qp += 1.0 / TEMPLATE_LENGTH
        if qp > quadrants[q][0]:
            q += 1
            qp = 0.0

    return template

def linearTemplate(quadrants):
    q = 0
    qp = 0.0
    val = 0.0
    qlp = 0

    template = []

    for i in range(TEMPLATE_LENGTH):
        quadrant = quadrants[q];
        ql = quadrant[0] * TEMPLATE_LENGTH
        template.append(val + ((quadrant[1] - val) * (i - qlp) / ql))

        qp += 1.0 / TEMPLATE_LENGTH
        if qp > quadrant[0]:
            val = quadrant[1]
            q += 1
            qp = 0.0
            qlp += ql

    return template

def sineTemplate(quadrants):
    q = 0
    qp = 0.0
    val = 0.0
    qlp = 0

    template = []

    for i in range(TEMPLATE_LENGTH):
        quadrant = quadrants[q];
        ql = quadrant[0] * TEMPLATE_LENGTH
        template.append(math.sin((q * 0.5 * math.pi) + (0.5 * math.pi * (i - qlp) / ql)))

        qp += 1.0 / TEMPLATE_LENGTH
        if qp > quadrant[0]:
            val = quadrant[1]
            q += 1
            qp = 0.0
            qlp += ql

    return template

class TemplateProvider():
    def __init__(self, templateFunction):
        self.sweepSteps = int(1000 * (1.0 + (5 * random.random())))
        self.pos = 0
        self.up = True
        self.startQuadrants = genQuadrants()
        self.endQuadrants = genQuadrants()
        self.tfn = templateFunction
        self.current = []
        self.step()

    def _qvOf(self, i):
        coeff = self.pos * 1.0 / self.sweepSteps
        return (self.startQuadrants[i][0] * (1.0 - coeff)) + (self.endQuadrants[i][0] * coeff)

    def get(self):
        return self.current

    def step(self):
        q = [(self._qvOf(i), self.startQuadrants[i][1]) for i in range(4)]
        self.current = self.tfn(q)

        if self.pos == self.sweepSteps:
            self.up = False
        elif self.pos == 0:
            self.up = True

        self.pos += 1 if self.up else -1


class WaveIterator():
    def __init__(self, template, f, vol, isCentreFreq):
        self.isCentreFreq = isCentreFreq
        self.template = template
        self.length = len(template.get())
        self.stretch = f * self.length / SAMPLE_RATE
        self.pos = 0
        self.vol = vol
    
    def next(self):
        ps = math.modf(self.pos * self.stretch)
        p = ps[0] + (int(ps[1]) % self.length)
        p0 = math.floor(p)
        p1 = (p0 + 1) if p0 < (self.length - 1) else 0
        if p1 == 0 and self.isCentreFreq:
            self.template.step()

        v0 = self.template.get()[p0]
        v1 = self.template.get()[p1]

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


    
def maxdDetuneCoeffAt(f):
    c = 0.012
    m = (c - 0.005) / 1000.0
    return c - (m * f)

def assembleWaves(f, template):
    waves = []
    waves.append(WaveIterator(template, f, 0.6 + (0.4 * random.random()), True))

    for i in range(random.randint(1, 3)):
        r = maxdDetuneCoeffAt(f) * random.random()
        fmUp = f * (1 + r)
        fmDown = f * (1 - r)
        waves.append(WaveIterator(template, fmUp, 0.6 + (0.4 * random.random()), False))
        waves.append(WaveIterator(template, fmDown, 0.6 + (0.4 * random.random()), False))

    return waves

def chooseTemplateFrom(frq):
    likelySmallNumber = 1.0 / pow(0.015 * frq, 1.5)
    dice = random.random()
    if dice > likelySmallNumber:
        return ("si", PureSineTemplate()) if dice > 0.5 else ("siq", TemplateProvider(sineTemplate))

    if dice > (0.5 * likelySmallNumber):
        return ("li", TemplateProvider(linearTemplate))

    if dice > (0.0 * likelySmallNumber):
        return ("sq", TemplateProvider(squareTemplate))

    # not possible for now
    # this must work differently, otherwise a new random waveshape will be applied to each cycle
    return ("ge", TemplateProvider(randomTemplateFrom, quadrants)) 

class Builder():
    def __init__(self, outDir, maxPoolSize):
        self.maxPoolSize = maxPoolSize
        self.buildDir = os.path.join(outDir, "factory")
        self.outDir = os.path.join(outDir, "raw")

    def build(self, n):
        files = [os.path.join(self.outDir, f) for f in os.listdir(self.outDir)]
        alreadyInPool = len(files)
        print(alreadyInPool, "files in pool already")
        if alreadyInPool >= self.maxPoolSize:
            print("enough")
            return

        frq = freq(n)
        (pref, templateProvider) = chooseTemplateFrom(frq)
        fn = "%d_%s_%s.wav" % (n, pref, datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S"))
        waves = assembleWaves(frq, templateProvider)
        durSecs = 2 + (10 * random.random())

        denominator = len(waves)
        data = []

        for i in range(int(SAMPLE_RATE * durSecs)):
            v = 0.0
            for w in waves:
                v += w.next()

            data.append(v / denominator)

        print("writing", fn)
        fqfn = os.path.join(self.buildDir, fn)
        sf.write(fqfn, Loopable(data).create(), SAMPLE_RATE)
        print("moving to pool")
        shutil.move(fqfn, self.outDir)


import time

def keyConfig():
    with open(sys.argv[2]) as conf:
        return json.load(conf)


def config():
    with open(sys.argv[3]) as conf:
        return json.load(conf)

outDir = sys.argv[1]
c = config()
maxPoolSize = int(c["maxSynthPoolSize"]) if "maxSynthPoolSize" in c else 30
builder = Builder(outDir, maxPoolSize)

i = 0
octave = 0
start = time.time()
while True:
    key = keyConfig()
    tonic = key["tonic"]
    mode = key["mode"]
    notes = [tonic]
    for m in range(len(mode)):
        notes.append(notes[m] + mode[m])

    n = notes[i]
    builder.build(n + (octave * 12))
    i += 1
    if i == len(notes):
        i = 0
        octave += 1
        if octave > 2:
            octave = -1

    while (time.time() - start) < 20:
        time.sleep(1)

    now = time.time()
    print("generation cycle took %0.1fs" % (now - start))
    start = now
