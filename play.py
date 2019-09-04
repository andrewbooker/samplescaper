#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math

import random
import queue
import time
import threading
sd.default.samplerate = 44100
sd.default.channels = 2


class RawSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = data

class Sample():
    def __init__(self, data):
        self.pos = 0
        pan = random.random()
        
        self.sampleCount = len(data)
        ramp = math.floor(self.sampleCount / (6.0 * random.random()))
        rampDown = self.sampleCount - ramp
        
        self.buffer = []
        for p in range(self.sampleCount):
            sample = data[p]
            if (p < ramp):
                sample *= (1.0 * p / ramp)

            if (p > rampDown):
                sample *= (1.0 - ((p - rampDown) / (1.0 * ramp)))

            left = sample * pan
            right = sample * (1.0 - pan)
            self.buffer.append([left, right])

    def hasData(self):
        return self.pos < self.sampleCount
        
    def readOne(self):
        self.pos += 1
        return self.buffer[self.pos - 1]
    
file = RawSample(sys.argv[1])
print("loading samples")
sample1 = Sample(file.data)
sample2 = Sample(file.data)
sample3 = Sample(file.data)
sample4 = Sample(file.data)
print("samples loaded")


class SampleMix():
    def __init__(self):
        self.samples = []
        
    def add(self, sample):
        self.samples.append(sample)
        
    def hasData(self):
        sum = 0
        for s in self.samples:
            sum += 1 if s.hasData() else 0
        return sum > 0
        
    def read(self, n):
        out = []

        for _ in range(n):
            stereoPair = [0.0, 0.0]
            toRemove = []
            for s in self.samples:
                if not s.hasData():
                    toRemove.append(s)
                else:
                    p = s.readOne()
                    stereoPair[0] += p[0]
                    stereoPair[1] += p[1]

            for s in toRemove:
                self.samples.remove(s)
                
            out.append(stereoPair)
        return out

mix = SampleMix()
mix.add(sample1)

reads = 0

q = queue.Queue()

def callback(outdata, frames, time, status):
    outdata[:] = q.get()
    
blocksize = 4410
q.put(mix.read(blocksize))
try:
    stream = sd.OutputStream(blocksize=blocksize, dtype="float32", callback=callback)
    with stream:

        while mix.hasData():
            q.put(mix.read(blocksize))
            reads += 1
            if (reads == 5):
                mix.add(sample2)
            if (reads == 15):
                mix.add(sample3)
            if (reads == 30):
                mix.add(sample4)
        print("finished reading after %d reads" % reads)
        time.sleep(60)
except KeyboardInterrupt:
    print("stopping")
print("done")

