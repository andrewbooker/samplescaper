#!/usr/bin/env python

import soundfile as sf
import sys
import math
import os

def createLoopableSample(raw):
    length = len(raw.data)
    halfWay = math.floor(len(raw.data) / 2)
    xFade = math.floor(0.75 * halfWay)
    data = []

    for s in range(length - xFade):
        p = s + xFade - halfWay
        if s < (halfWay - xFade):
            data.append(raw.data[s + halfWay])
        elif s >= halfWay:
            data.append(raw.data[p])
        else:
            f = 1.0 * p / xFade
            data.append((f * raw.data[p]) + ((1.0 - f) * raw.data[s + halfWay]))

    return data
    

class RawSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = []
        for d in range(2 * 4410, len(data) - (5 * 4410)):
            self.data.append(data[d])


inDir = sys.argv[1]
outDir = sys.argv[2]
lowestNote = int(sys.argv[3]) if len(sys.argv) > 3 else 48
highestNote = int(sys.argv[4]) if len(sys.argv) > 4 else 69

for noteNumber in range(lowestNote, highestNote + 1):
    noteDir = os.path.join(inDir, str(noteNumber))
    noteFiles = [f for f in os.listdir(noteDir) if os.path.isfile(os.path.join(noteDir, f))]
    fileOutDir = os.path.join(outDir, str(noteNumber))
    if not os.path.exists(fileOutDir):
        os.makedirs(fileOutDir)
    i = 0
    for f in noteFiles:
        outFile = os.path.join(fileOutDir, "loop_%d_%d.wav" % (noteNumber, i))
        print("converting %s into %s" % (f, outFile))
        rawSample = RawSample(os.path.join(noteDir, f))

        sf.write(outFile, createLoopableSample(rawSample), 44100)
        i += 1
