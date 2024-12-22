#!/usr/bin/env python

import sys
import sounddevice as sd
import soundfile as sf
import struct
import numpy as np
import random
import os
import time


device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 0.3
inDir = sys.argv[3] if len(sys.argv) > 3 else None
playingTimeMins = int(sys.argv[4]) if len(sys.argv) > 4 else 3

if device is None:
    print(sd.query_devices())
    exit()


samplerate = 44100
blocksize = 4096
maxLeadInSecs = 5


class MonoSoundSource:
    def __init__(self):
        self.pos = 0

    def advance(self, size):
        self.pos += size
        pass


class MonoSineSource(MonoSoundSource):
    def __init__(self, freq):
        super(MonoSineSource, self).__init__()
        self.freq = freq

    def read(self, size):
        t = (self.pos + np.arange(size)) / samplerate
        super().advance(size)
        return level * np.sin(2 * np.pi * self.freq * t.reshape(-1, 1))

    def isFinished(self):
        return False


class MonoWavSource(MonoSoundSource):
    def __init__(self, inDir):
        super(MonoWavSource, self).__init__()
        self.inDir = inDir
        self.fileBuffer = None
        self.blockCountdown = 0
        self.start = time.time()
        self.hasFinished = False

    def isFinished(self):
        return self.hasFinished

    def getFile(self):
        rawFiles = os.listdir(self.inDir)
        if len(rawFiles) == 0:
            return
        random.shuffle(rawFiles)
        selected = rawFiles[0]
        leadIn = maxLeadInSecs * random.random()
        print(selected, f"in {leadIn:.2f}s")
        data, _ = sf.read(os.path.join(self.inDir, selected))
        self.fileBuffer = [0.0] * int(leadIn * samplerate)
        self.fileBuffer.extend([(level * d) for d in data])

    def read(self, size):
        if self.fileBuffer is None:
            if (time.time() - self.start) > (60 * playingTimeMins):
                self.hasFinished = True
            else:
                self.getFile()
            return [0.0] * size

        ret = self.fileBuffer[self.pos:self.pos + size]
        super().advance(size)
        shortfall = size - len(ret)
        if shortfall < 1:
            return ret
        self.fileBuffer = None
        self.pos = 0
        return [ret[i] if i < len(ret) else 0.0 for i in range(size)]


sources = []

if inDir is not None:
    sources.extend([
        MonoWavSource(inDir),
        MonoWavSource(inDir)
    ])
else:
    sources.extend([
        MonoSineSource(220),
        MonoSineSource(441)
    ])

def callback(outdata, frames, time, status):
    if status:
        print(status, file=sys.stderr)

    block = np.dstack([s.read(frames) for s in sources]).flatten()
    outdata[:] = struct.pack(f"{len(sources) * frames}f", *block)

with sd.RawOutputStream(samplerate=samplerate, blocksize=blocksize, device=device, channels=len(sources), dtype="float32", callback=callback):
    while True:
        if all([s.isFinished() for s in sources]):
            print("All sources finished")
            exit(0)
        time.sleep(3)


