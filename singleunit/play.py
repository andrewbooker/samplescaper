#!/usr/bin/env python

import sys
import sounddevice as sd
import struct
import numpy as np


device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 0.3

if device is None:
    print(sd.query_devices())
    exit()


samplerate = 44100
channels = 2
blocksize = 1024

class MonoSoundSource:
    def __init__(self, freq):
        self.pos = 0
        self.freq = freq

    def read(self, size):
        t = (self.pos + np.arange(size)) / samplerate
        self.pos += size
        return level * np.sin(2 * np.pi * self.freq * t.reshape(-1, 1))

source1 = MonoSoundSource(220)
source2 = MonoSoundSource(441)

def callback(outdata, frames, time, status):
    if status:
        print(status, file=sys.stderr)

    block = np.dstack((
        source1.read(frames),
        source2.read(frames)
    )).flatten()
    outdata[:] = struct.pack(f"{frames * channels}f", *block)

with sd.RawOutputStream(samplerate=samplerate, blocksize=blocksize, device=device, channels=channels, dtype="float32", callback=callback):
    input()


