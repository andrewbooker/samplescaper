#!/usr/bin/env python

import soundfile as sf
import sys
import math
import os

class RawSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = data


def createLoopableSample(raw):
    halfWay = math.floor(len(raw.data) / 2)
    xFade = math.floor(halfWay / 2)
    data = []

    for s in range(len(raw.data) - xFade):
        p = s + xFade - halfWay
        if s < (halfWay - xFade):
            data.append(raw.data[s + halfWay])
        elif s >= halfWay:
            data.append(raw.data[p])
        else:
            f = 1.0 * p / xFade
            data.append((f * raw.data[p]) + ((1.0 - f) * raw.data[s + halfWay]))

    return data


inDir = sys.argv[1]
outDir = sys.argv[2]

for noteNumber in range(48, 70):
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
