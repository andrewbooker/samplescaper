#!/usr/bin/env python

import sys
import os
import random
import soundfile as sf
import time
import shutil
import json


def outArr(xfBuff, buff):
    l = len(xfBuff)
    if l == 0:
        return buff

    xb = [((1.0 - (float(i) / l)) * xfBuff[i]) + (float(i) * buff[i] / l) for i in range(l)]
    return xb + buff[l:]

def anyOf(a):
    return a[random.randint(0, len(a) - 1)]

def config():
    with open(sys.argv[2]) as conf:
        return json.load(conf)


inDir = sys.argv[1]
rawDir = os.path.join(inDir, "raw")
factoryDir = os.path.join(inDir, "factory")
outDir = os.path.join(inDir, "raw")
sampleRate = 44100
conf = config()
numsToUse = conf["arpeggiate"]
maxPoolSize = int(conf["maxArpeggiatorPoolSize"]) if "maxArpeggiatorPoolSize" in conf else 30

class NoteChooser():
    def up(self):
        return []

    def down(self):
        return []

class CombUpDown(NoteChooser):
    def __init__(self, allFiles, numberToUse):
        toUseByNote = [(int(f.split("_")[0]), f) for f in allFiles[:numberToUse]]

        toUseByNote.sort(key=lambda f: f[0])
        sortedFiles = [f[1] for f in toUseByNote]
        self.u = sortedFiles[::2]
        self.d = sortedFiles[1::2]
        if random.random() > 0.5:
            self.d.reverse()

    def up(self):
        return self.u

    def down(self):
        return self.d

class Arpeggiator():
    def __init__(self, maxPoolSize):
        self.maxPoolSize = maxPoolSize
        self.done = []
        self.fIdx = 0

    def run(self):
        allFiles = os.listdir(rawDir)
        rawFiles = [f for f in filter(lambda fn: "arpeggiated" not in fn, allFiles)]
        arpeggiatedFiles = [f for f in filter(lambda fn: "arpeggiated" in fn, allFiles)]
        numberOfArp = len(arpeggiatedFiles)
        numberOfRaw = len(rawFiles)
        if numberOfArp > numberOfRaw:
            print("already have more arpeggiated than raw files")
            return
        if (numberOfRaw + numberOfArp) > 30:
            print("enough files in pool already")
            return

        numToUse = anyOf(numsToUse)
        if numberOfRaw < numToUse:
            print("insufficient files to arpeggiate", numToUse)
            return
        print("using", numToUse, "files")
        
        random.shuffle(rawFiles)
        chooser = CombUpDown(rawFiles, numToUse)
        toUse = chooser.up() + chooser.down()

        inf = [sf.read(os.path.join(rawDir, f))[0] for f in toUse]
        inFiles = [[len(f), f, 0] for f in inf]

        unitSampleLength = int(sampleRate * (0.05 + (0.2 * random.random())))
        xfadeLength = int(0.03 + (0.4 * random.random()) * unitSampleLength)
        print("unitSampleLength", unitSampleLength)
        pulses = random.randint(18, 50)
        print(pulses, "pulses")

        fqfn = os.path.join(factoryDir, "arpeggiated_%04d.wav" % self.fIdx)
        outfile = sf.SoundFile(fqfn, "w", samplerate=sampleRate, channels=1)

        print("writing", fqfn)
        xfBuff = []
        for p in range(pulses):
            for fd in inFiles:
                start = fd[2]
                end = start + unitSampleLength
                outfile.write(outArr(xfBuff, [fd[1][s] for s in range(start, end)]))
                xfBuff = [fd[1][s] for s in range(end, end + xfadeLength)]
                fd[2] += unitSampleLength
                if (fd[2] + unitSampleLength + xfadeLength) > fd[0]:
                    fd[2] = 0

        outfile.close()
        shutil.move(fqfn, outDir)
        print("moved to live pool")
        self.fIdx += 1
        self.done.append(os.path.join(outDir, fqfn))

arp = Arpeggiator(maxPoolSize)
while True:
    arp.run()
    time.sleep(10)
