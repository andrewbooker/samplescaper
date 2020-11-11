#!/usr/bin/env python


import soundfile as sf
import sys
import os


SAMPLE_RATE = 44100

class AudioFile():
    def __init__(self, fqfn, startTime):
        self.fqfn = fqfn
        self.start = startTime
        self.file = sf.SoundFile(fqfn, "r")
        self.done = False

    def __del__(self):
        self.file.close()

    def _read(self, length):
        return [[s[0], s[1]] for s in self.file.read(length)]

    def occursInBlockStarting(self, t):
        return not self.done and ((t + 1) > self.start)

    def nextBlock(self, fromT):
        if fromT < self.start:
            print("starting with", self.fqfn, "at", fromT)
            pre = int(SAMPLE_RATE * (self.start - fromT))
            buff = [[0.0,0.0]] * pre
            return buff + self._read(SAMPLE_RATE - pre)
        else:
            d = self._read(SAMPLE_RATE)
            l = len(d)
            if l < SAMPLE_RATE:
                self.done = True
                print("done with", self.fqfn, "at", fromT)
                return d + ([[0.0,0.0]] * (SAMPLE_RATE - l))

            return d

def merge(b1, b2):
    return [[b1[i][0] + b2[i][0], b1[i][1] + b2[i][1]] for i in range(len(b1))]


def parseLof(fqfn, substituteDir):
    files = []
    with open(fqfn, "r") as f:
        ls = f.readlines()
        for l in ls:
            s = float(l.split(" offset ")[1])
            f = os.path.join(substituteDir, os.path.basename(l.split("\"")[1]))
            files.append((f, s))
    return files


inDir = sys.argv[1]
outFqFn = sys.argv[2]
files = parseLof(os.path.join(inDir, "inventory.lof"), inDir)

done = (len(files) == 0)
audioFiles = [AudioFile(f[0], f[1]) for f in files]
t = 0
with sf.SoundFile(outFqFn, "w", samplerate=SAMPLE_RATE, channels=2) as outFile:
    started = False
    while not done:
        doneAll = True
        b = [[0.0,0.0]] * SAMPLE_RATE
        for f in audioFiles:
            if not f.done:
                doneAll = False
                if f.occursInBlockStarting(t):
                    started = True
                    b = merge(f.nextBlock(t), b)

        outFile.write(b)
        t += 1
        done = started and doneAll

for a in audioFiles:
    del a
