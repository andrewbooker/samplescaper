#!/usr/bin/env python

import soundfile as sf
import math

class LoopableSample():
    def __init__(self):
        self.data = []
        
    def addBuffer(self, buffer):
        for d in buffer:
            self.data.append(d)

    def fromFile(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.addBuffer(data[2 * 4410:])
        return self

    def length(self):
        return len(self.data) - (5 * 4410)
            
    def create(self, outFile):
        l = self.length()
        halfWay = math.floor(l / 2)
        xFade = math.floor(0.75 * halfWay)
        out = []

        for s in range(l - xFade):
            p = s + xFade - halfWay
            if s < (halfWay - xFade):
                out.append(self.data[s + halfWay])
            elif s >= halfWay:
                out.append(self.data[p])
            else:
                f = 1.0 * p / xFade
                out.append((f * self.data[p]) + ((1.0 - f) * self.data[s + halfWay]))

        sf.write(outFile, out, 44100)
