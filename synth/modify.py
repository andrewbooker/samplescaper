#!/usr/bin/env python

import soundfile as sf
import sys
import os
import random
import math
import time
import json


def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: ("wav" in f) and (("si" in f) or ("arpeggiated" in f)), os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])
    
def stretch():
    return 0.05 + (0.1 * (random.random() - 0.5))
    
def noteFrom(fn, tonic):
    spl = fn.split("_")
    if spl[0].isdigit():
        return int(spl[0])
    return tonic

def freq(n):
    return math.pow(2, (n - 69)/12.0) * 440

def anyIn(a, excluding):
    i = random.randint(0, len(a) - 1)
    n = a[i]
    if n == excluding:
        return anyIn(a, excluding)
    return n


class Linear():
    def __init__(self, note, noteRange):
        octave = note in [tonic, tonic + 12] or random.random() > 0.9
        maxRampTime = 0.3
        self.start = 1
        if octave:
            down = random.random() > 0.6 and note > 67
            if down:
                maxRampTime = 0.1
            self.end = 0.5 if down else 2.0
        else:
            to = anyIn(noteRange, note)
            print("from", note, "to", to)
            self.end = freq(to) / freq(note)
            print(self.end)

        self.gradient = 0
        self.bufferStart = 0.1 + (0.3 * random.random())
        rampLength = 0.01 + (maxRampTime * random.random())
        self.bufferEnd = 1.0 - self.bufferStart - rampLength
        self.startAt = 0

    def over(self, dataLength):
        self.startAt = dataLength * self.bufferStart
        dl = dataLength * (1 - (self.bufferStart + self.bufferEnd))
        self.gradient = (self.end - self.start) / dl
        return self

    def at(self, i):
        if i < self.startAt:
            return self.start
        v = self.start + ((i - self.startAt) * self.gradient)
        if (self.end > self.start and v > self.end) or (self.end < self.start and v < self.end):
           return self.end
        return v

    def describe(self):
        return ("lin_%3f" % (self.end)).replace(".", "_")

class Sine():
    def __init__(self, dataLength):
        self.dataLength = dataLength
        self.phase = 0
        self.amplitude = 0.001 + (0.01 * random.random())
        self.descr = ""

    def forDetune(self):
        self.freq = 2.0 * math.pi / self.dataLength
        self.phase = 2 * random.random() * math.pi
        self.desc = "%.3f" % self.amplitude
        return self

    def forFreqMod(self):
        f = 200 + (50000 * random.random())
        self.freq = f / self.dataLength
        self.desc = "%.0f" % f
        self.amplitude = 0.1 + (0.9 * random.random())
        return self

    def at(self, i):
        return 1.0 + (self.amplitude * math.sin(self.phase + (i * self.freq)))

    def describe(self):
        return "sin_%s" % self.desc

class Effect():
    def __init__(self):
        self.description = ""

    def describe(self):
        return self.description


def applyPitchTo(data, sampleRate, dataLength, eff):
    out = []

    done = False
    i = 0
    p = 0
    while not done:
        p += eff.at(i)
        p0 = math.floor(p)
        if p0 >= dataLength or i > (30 * sampleRate):
            done = True
        else:
            p1 = math.ceil(p)
            if p1 >= dataLength:
                p1 = p0
            e = p - p0
            out.append(((1.0 - e) * data[p0]) + (e * data[p1]))
        i += 1

    return out

class Sweep(Effect):
    def __init__(self, fn, noteRange):
        self.note = noteFrom(fn, noteRange[0])
        self.noteRange = noteRange

    def appliedTo(self, data, sampleRate):
        dataLength = len(data)
        eff = Linear(self.note, self.noteRange).over(dataLength)
        self.description = eff.describe()
        return applyPitchTo(data, sampleRate, dataLength, eff)

class Detune(Effect):
    def appliedTo(self, data, sampleRate):
        dataLength = len(data)
        eff = Sine(dataLength).forDetune()
        self.description = eff.describe()
        return applyPitchTo(data, sampleRate, dataLength, eff)

class FreqMod(Effect):
    def appliedTo(self, data, sampleRate):
        dataLength = len(data)
        eff = Sine(dataLength).forFreqMod()
        self.description = eff.describe()
        return applyPitchTo(data, sampleRate, dataLength, eff)

class Multiply(Effect):
    def __init__(self, fn):
        self.other = []
        self.otherFn = ""

        poolDir = os.path.dirname(fn)
        spl = os.path.basename(fn).split("_")
        note = None
        if len(spl) > 1 and spl[0].isnumeric():
            note = int(spl[0])
        files = [f for f in filter(lambda f: f not in fn and (f.startswith(str(note)) or f.startswith(str(note + 12)) or f.startswith(str(note - 12))), os.listdir(poolDir))]

        if len(files) > 0:
            self.otherFn = files[random.randint(0, len(files) - 1)]
            self.other = sf.read(os.path.join(poolDir, self.otherFn))[0]
            print("multiplying with", self.otherFn)

    def appliedTo(self, data, sampleRate):
        out = []
        o = len(self.other)
        self.description = "multiply"
        ld = min(len(data), len(self.other)) if o > 0 else len(data)
        for d in range(ld):
            m = abs(self.other[d]) if o > 0 else data[d]
            out.append(data[d] * m)
        return out


def config():
    with open(sys.argv[3]) as conf:
        return json.load(conf)

inDir = sys.argv[1]
outDir = sys.argv[2]
effName = sys.argv[4] if len(sys.argv) > 4 else "Sweep"
xFade = 441

while True:
    f = nextAudioFileFrom(inDir)

    if f is not None:
        print("using", os.path.basename(f))
        data, sampleRate = sf.read(f)
        effect = None

        if effName == "Sweep":
            conf = config()
            tonic = conf["tonic"]
            mode = conf["mode"]
            notes = [tonic]
            for m in range(len(mode)):
                notes.append(notes[m] + mode[m])
            print("based on note range", notes)
            effect = Sweep(os.path.basename(f), notes)
        elif effName == "Detune":
            effect = Detune()
        elif effName == "FreqMod":
            effect = FreqMod()

        if effect is not None:
            out = effect.appliedTo(data, sampleRate)

            g = 1.0 / xFade
            l = len(out)
            sample = out[xFade:-xFade] + [(out[-xFade:][s] * (1.0 - (g * s))) + (out[s] * g * s) for s in range(0, xFade)]
            fn = "%s_%s.wav" % (os.path.basename(f).split(".")[0], effect.describe())
            sf.write(os.path.join(outDir, fn), sample, sampleRate)
            print("written", fn)
        else:
            print("no modification for", effName)

    if len(os.listdir(inDir)) > 30:
        print("dropping", f)
        os.remove(f)

    time.sleep(10)

