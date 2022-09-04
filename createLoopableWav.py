#!/usr/bin/env python

import sys
import os

from capture.LoopableSample import LoopableSample

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
        LoopableSample().fromFile(os.path.join(noteDir, f)).create(outFile)
        i += 1
