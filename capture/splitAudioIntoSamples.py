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
sampleAvg = SampleAvg(44100, 10)
state = State.SILENT

sample = []
print("splitting", inFile)
(data, sampleRate) = sf.read(inFile)
for d in range(len(data)):
    val = abs(data[d])
    if state == State.IN_SAMPLE:
        sampleAvg.add(val)
        if val < 0.1 and sampleAvg.canAssess() and sampleAvg.peak() < 0.1:
            state = State.SILENT
            sampleAvg.clear()
            if len(sample) > sampleAvg.avgLen:
                writer.write(sample[:-sampleAvg.avgLen])
                sample = []
        else:
            sample.append(data[d])
    if state == State.SILENT and val > 0.2:
        print("first rise at", 1.0 * d / sampleRate)
        state = State.IN_SAMPLE
    

