#!/usr/bin/env python

import sys
import sounddevice as sd
import soundfile as sf
import struct
import numpy as np
import random
import os
import time
import threading


device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 0.3
inDir = sys.argv[3] if len(sys.argv) > 3 else None
channels = int(sys.argv[4]) if len(sys.argv) > 4 else 3
playingTimeMins = float(sys.argv[5]) if len(sys.argv) > 5 else 3

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


class AudioFileLoader:
    def __init__(self, inDir):
        self.inDir = inDir
        self.currently_loading_for = None
        self.loading = None

    def getFile(self):
        rawFiles = os.listdir(self.inDir)
        if len(rawFiles) == 0:
            return
        random.shuffle(rawFiles)
        selected = rawFiles[0]
        leadIn = maxLeadInSecs * random.random()
        data, _ = sf.read(os.path.join(self.inDir, selected))
        self.currently_loading_for.fileBuffer = [0.0] * int(leadIn * samplerate)
        self.currently_loading_for.fileBuffer.extend([(level * d) for d in data])

    def start_loading_to(self, source):
        self.currently_loading_for = source
        self.loading = threading.Thread(target=self.getFile, daemon=True)
        self.loading.start()

    def request_file_for(self, source):
        if self.currently_loading_for is None:
            self.start_loading_to(source)
            print("loading started for", self.currently_loading_for.me)
            return False

        if self.currently_loading_for.me != source.me:
            return False

        if self.loading is not None and self.loading.is_alive():
            return False

        self.loading.join()
        self.loading = None
        print("finished loading", self.currently_loading_for.me)
        self.currently_loading_for.is_ready = True
        self.currently_loading_for = None
        return True


loader = AudioFileLoader(inDir)


class MonoWavSource(MonoSoundSource):
    def __init__(self, loader, idx):
        super(MonoWavSource, self).__init__()
        self.loader = loader
        self.fileBuffer = None
        self.start = time.time()
        self.hasFinished = False
        self.me = idx
        self.is_ready = False

    def isFinished(self):
        return self.hasFinished

    def read(self, size):
        if not self.is_ready:
            empty = [0.0] * size
            if (time.time() - self.start) > (60 * playingTimeMins):
                self.hasFinished = True
                return empty

            self.loader.request_file_for(self)
            return empty

        ret = self.fileBuffer[self.pos:self.pos + size]
        available = len(ret)
        super().advance(size)
        if available == size:
            return ret
        print("reached the end of", self.me)
        self.is_ready = False
        self.pos = 0
        empty = [0.0] * size
        empty[:available] = ret
        return empty


sources = []

if inDir is not None:
    sources.extend([
        MonoWavSource(loader, i) for i in range(channels)
    ])
else:
    sources.extend([
        MonoSineSource(220),
        MonoSineSource(441)
    ])

print("playing", len(sources), "sources")

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


