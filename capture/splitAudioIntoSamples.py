#!/usr/bin/env python

import sys
import os
import soundfile as sf
from enum import Enum
from lib.sampleAvg import SampleAvg

class Writer:
    def __init__(self, outDir):
        self.idx = 0
        self.outDir = outDir

    def write(self, data):
        fn = f"{self.idx}.wav"
        fqfn = os.path.join(self.outDir, fn.rjust(7, "0"))
        print("writing", fqfn)
        sf.write(fqfn, data, 44100)
        self.idx += 1

        

class State(Enum):
    SILENT = 1
    IN_SAMPLE = 2

inFile = sys.argv[1]
outDir = sys.argv[2]
writer = Writer(outDir)
sampleAvg = SampleAvg(4410, 0)
state = State.SILENT
lastStatePos = 0

sample = []
print("splitting", inFile)
(data, sampleRate) = sf.read(inFile)
for d in range(len(data)):
    val = abs(data[d])
    sampleAvg.add(val)
    if state == State.IN_SAMPLE:
        if sampleAvg.val() < 0.05 and (d - lastStatePos) > sampleRate:
            print("exit at", 1.0 * d / sampleRate)
            state = State.SILENT
            lastStatePos = d
            if len(sample) > sampleAvg.avgLen:
                writer.write(sample[:-sampleAvg.avgLen])
                sample = []
        else:
            sample.append(data[d])
    if state == State.SILENT and sampleAvg.val() > 0.1 and (d - lastStatePos) > sampleRate:
        print("entry at", 1.0 * d / sampleRate)
        state = State.IN_SAMPLE
        lastStatePos = d
    

