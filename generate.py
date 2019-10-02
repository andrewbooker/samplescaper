#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math
import random

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
        
    def reset(self):
        self.pos = 0
        
    def canStart(self):
        return self.pos == 0
        
    def readOne(self):
        self.pos += 1
        return self.buffer[self.pos - 1]
    
files = []
files.append(RawSample(sys.argv[1]))
print("loading samples")
samples = []
for file in files:
    for _ in range(12):
        samples.append(Sample(file.data))
print("%d samples loaded" % len(samples))


def nextSample():
    s = samples[random.randint(0, len(samples) - 1)]
    if s.canStart():
        return s
    return None
    
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
                s.reset()
                self.samples.remove(s)
                
            out.append(stereoPair)
        return out

mix = SampleMix()
mix.add(nextSample())
    
blocksize = 4410
durationMins = int(sys.argv[2]) if len(sys.argv) > 2 else 1
blocksToWrite = int(durationMins * 600)

with sf.SoundFile("./test.wav", "w", samplerate=sd.default.samplerate, channels=2) as outfile:
    for _ in range(blocksToWrite):
        outfile.write(mix.read(blocksize))
        if (blocksToWrite % 10) == 0:
            next = nextSample()
            if next is not None:
                mix.add(next)
            

print("done")

