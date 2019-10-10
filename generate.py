#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math
import random
import time
from datetime import datetime
from utils.AvailableSamples import files

sd.default.samplerate = 44100
sd.default.channels = 2


class RawSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = data

class Sample():
    def __init__(self, data):
        self.data = data
        self.sampleCount = len(data)

        self.pan = 0.5
        self.pos = 0
        self.requiredLength = 9
        self.requiredPos = 0
        self.rampUp = 9
        self.rampDown = 9
        self.reset()

    def reset(self):
        self.pos = 0
        self.requiredPos = 0
        self.requiredLength = 44100 * 10 * (0.1 + random.random())
        self.rampUp = math.floor(self.requiredLength / (6.0 * (0.1 + random.random())))
        self.rampDown = math.floor(self.requiredLength / (4.0 * (0.5 + random.random())))
        self.pan = random.random()
        
    def hasData(self):
        return self.requiredPos < self.requiredLength

    def canStart(self):
        return self.requiredPos == 0
        
    def readOne(self):
        self.requiredPos += 1
        self.pos = self.pos + 1 if self.pos < self.sampleCount else 1
        sample = self.data[self.pos - 1]
        if (self.requiredPos < self.rampUp):
            sample *= (1.0 * self.requiredPos / self.rampUp)

        rampDownStart = self.requiredLength - self.rampDown
        if (self.requiredPos > rampDownStart):
            sample *= (1.0 - ((self.requiredPos - rampDownStart) / (1.0 * self.rampDown)))

        left = sample * self.pan
        right = sample * (1.0 - self.pan)
        return [left, right]
    

print("loading files for %d notes" % len(files))
loadedFiles = {}
l = 0
for n in files.keys():
    loadedFiles[n] = [RawSample(f) for f in files[n]]
    l += len(loadedFiles[n])
print("%d files loaded" % l)
start = time.time()
notes = []
notes[:] = loadedFiles.keys()


class NotePlayer():
    blockInterval = 3
    maxInflightPerNote = 6

    def __init__(self, mix):
        self.mix = mix
        self.lastBlock = 0
        self.currentNotes = []

    def nextNote(self, note):
        self.currentNotes.append({"note": note, "added": 0})

    def tickBlock(self, b):
        if b - self.lastBlock == NotePlayer.blockInterval:
            for n in self.currentNotes:
                if n["added"] < NotePlayer.maxInflightPerNote:
                    available = loadedFiles[n["note"]]
                    f = available[random.randint(0, len(available) - 1)]
                    self.mix.add(Sample(f.data))
                    n["added"] += 1
                else:
                    self.currentNotes.remove(n)
            self.lastBlock = b

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

def secsUntilNextSample():
    return random.randint(2, 10);


mix = SampleMix()
notePlayer = NotePlayer(mix);
notePlayer.nextNote(notes[random.randint(0, len(notes) - 1)])
    
blocksize = 4410
durationMins = (int(sys.argv[4]) if len(sys.argv) > 4 else 3)
blocksToWrite = int(durationMins * 600)
print("writing %d blocks" % blocksToWrite)

lastBlockStart = 0
nextSampleAt = secsUntilNextSample() * 10

with sf.SoundFile("./test.wav", "w", samplerate=sd.default.samplerate, channels=2) as outfile:
    for b in range(blocksToWrite):
        outfile.write(mix.read(blocksize))
        notePlayer.tickBlock(b)

        if b > nextSampleAt:
            notePlayer.nextNote(notes[random.randint(0, len(notes) - 1)])
            nextSampleAt = b + (secsUntilNextSample() * 10)


timeTaken = time.time() - start
print("finished after %s" % datetime.fromtimestamp(timeTaken).strftime("%M:%S"))

