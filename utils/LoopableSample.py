#!/usr/bin/env python

import soundfile as sf
import math

class LoopableSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = []
        for d in range(2 * 4410, len(data) - (5 * 4410)):
            self.data.append(data[d])

    def create(self, outFile):
        length = len(self.data)
        halfWay = math.floor(len(self.data) / 2)
        xFade = math.floor(0.75 * halfWay)
        out = []

        for s in range(length - xFade):
            p = s + xFade - halfWay
            if s < (halfWay - xFade):
                out.append(self.data[s + halfWay])
            elif s >= halfWay:
                out.append(self.data[p])
            else:
                f = 1.0 * p / xFade
                out.append((f * self.data[p]) + ((1.0 - f) * self.data[s + halfWay]))

        sf.write(outFile, out, 44100)