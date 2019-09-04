#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math

import random
import queue

sd.default.samplerate = 44100
sd.default.channels = 2


class RawSample():
    def __init__(self, file):
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = data


class Sample():
    def __init__(self, data):
        self.data = data
        self.pan = random.random()
        self.pos = 0
        self.sampleCount = len(data)
        self.ramp = math.floor(self.sampleCount / (6.0 * random.random()))
        self.rampDown = self.sampleCount - self.ramp

    def hasData(self):
        return self.pos < self.sampleCount
        
    def readOne(self):
        if not self.hasData():
            return [0.0, 0.0]
        
        sample = self.data[self.pos]
        if (self.pos < self.ramp):
            sample *= (1.0 * self.pos / self.ramp)

        if (self.hasData() and (self.pos > self.rampDown)):
            sample *= (1.0 - ((self.pos - self.rampDown) / (1.0 * self.ramp)))

        left = sample * self.pan
        right = sample * (1.0 - self.pan)
        self.pos += 1
        return [left, right]
        
    def read(self, n):
        out = []
        for _ in range(n):
            out.append(readOne())

        return out
    
file = RawSample(sys.argv[1])
sample1 = Sample(file.data)
sample2 = Sample(file.data)


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
    
def finishedPlaying():
    print("finished")

blocksize = 4410
q.put(mix.read(blocksize))
stream = sd.OutputStream(blocksize=blocksize, latency=5.0, dtype="float32", callback=callback, finished_callback=finishedPlaying)


with stream:
    while mix.hasData():
        q.put(mix.read(blocksize))
        reads += 1
        if (reads == 5):
            mix.add(sample2)
    print("finished reading")
