#!/usr/bin/env python

import math
import soundfile as sf
import random
import sys
import os
import shutil
import time

class Envelope:
    def __init__(self, sampleRate):
        upSecs = 0.5 + (2 * random.random())
        downSecs = 5.0 + (5 * random.random())
        self.lengthSecs = upSecs + downSecs + (random.random() * 5.0)
        self.up = int(upSecs * sampleRate)
        self.downStart = int((self.lengthSecs - downSecs) * sampleRate)
        self.required = int(self.lengthSecs * sampleRate)
        self.down = self.required - self.downStart
        
    def vol(self, i):
        if i < self.up:
            return 1.0 * i / self.up
        if i > self.downStart:
            return 1.0 - (float(i - self.downStart) / self.down)
        return 1.0

class Pan:
    def __init__(self, sampleRate, numberOfFiles):
        self.freqHz = 10 * math.log(numberOfFiles + 1) * random.random()
        self.radPerSample = self.freqHz * 2 * math.pi / sampleRate
        self.offset = random.random() * sampleRate / self.freqHz

    def at(self, i):
        return 0.5 * (1.0 + math.sin((i + self.offset) * self.radPerSample))


class LoopFiles:
    sampleRate = 44100
    def __init__(self, inDir, factoryDir, outDir):
        self.inDir = inDir
        self.factoryDir = factoryDir
        self.outDir = outDir

    def chooseFrom(self, rawFiles):
        return []

    def convert(self, files):
        pass

    def envelope(self):
        env = Envelope(LoopFiles.sampleRate)
        print("creating %.2fs" % env.lengthSecs, "file of", env.required, "samples")
        return env


class LoopStereo(LoopFiles):
    def __init__(self, inDir, factoryDir, outDir):
        super(LoopStereo, self).__init__(inDir, factoryDir, outDir)

    @staticmethod
    def pannedSample(vals, pan):
        return [vals[0] * pan, vals[len(vals) - 1] * (1.0 - pan)]

    def chooseFrom(self, rawFiles):
        takeFirstTwo = (random.random() > 0.2) and (len(rawFiles) > 1)
        if takeFirstTwo:
            print("Creating paired loop")
            return rawFiles[:2]
        return super().chooseFrom(rawFiles)

    def convert(self, files):
        fqfns = [os.path.join(self.inDir, f) for f in files]
        fd = [sf.read(f)[0] for f in fqfns]
        fileData = [(len(d), d) for d in fd]

        env = self.envelope()
        pan = Pan(LoopFiles.sampleRate, len(files))
        start = time.monotonic()

        fqfn = os.path.join(self.factoryDir, "looped_%s" % "__".join(files))
        sf.write(fqfn, [LoopStereo.pannedSample([env.vol(i) * f[1][i % f[0]] for f in fileData], pan.at(i)) for i in range(0, env.required)], LoopFiles.sampleRate)
        print("moving to live pool after %.2fs" % (time.monotonic() - start))
        shutil.move(fqfn, self.outDir)


class LoopMono(LoopFiles):
    def __init__(self, inDir, factoryDir, outDir):
        super(LoopMono, self).__init__(inDir, factoryDir, outDir)

    def convert(self, files):
        fqfn = os.path.join(self.inDir, files[0])
        fd = sf.read(fqfn)[0]
        rawLength = len(fd)

        env = self.envelope()
        start = time.monotonic()

        fqfn = os.path.join(self.factoryDir, "looped_%s" % "__".join(files))
        sf.write(fqfn, [env.vol(i) * fd[i % rawLength] for i in range(0, env.required)], LoopFiles.sampleRate)
        print("moving to live pool after %.2fs" % (time.monotonic() - start))
        shutil.move(fqfn, self.outDir)


def convertItems(looper, rawFiles, outDir, batchSize):
    loopedFiles = [f[7:] for f in os.listdir(outDir)]
    if len(loopedFiles) > 100:
        print("Skipping looped file generation, have enough")
        return

    files = looper.chooseFrom(rawFiles)
    if len(files) == 2:
        try:
            looper.convert(files)
        except:
            print("failed to create paired loop, file probably still being written")
        return

    l = len(rawFiles)
    for f in rawFiles:
        if f not in loopedFiles:
            try:
                looper.convert([f])
            except:
                print("failed to create loop for", f, "probably still being written")
        else:
            print("already done", f)

    if l == batchSize:
        for f in rawFiles:
            fqfn = os.path.join(inDir, f)
            if os.path.exists(fqfn):
                print("looper deleting", f)
                os.remove(fqfn)
            else:
                print("looper about to delete", f, "but already deleted")

inDir = os.path.join(sys.argv[1], "raw")
factoryDir = os.path.join(sys.argv[1], "factory")
outDir = os.path.join(sys.argv[1], "looped")
BATCH_SIZE = 10
looping = LoopMono if len(sys.argv) > 2 and sys.argv[2] == "mono" else LoopStereo
looper = looping(inDir, factoryDir, outDir)

while True:
    rawFiles = os.listdir(inDir)
    random.shuffle(rawFiles)
    convertItems(looper, rawFiles[:BATCH_SIZE], outDir, BATCH_SIZE)
    time.sleep(3)

