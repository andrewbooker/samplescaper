#!/usr/bin/env python

import sys
import sounddevice as sd
import struct
import numpy as np


device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 1.0

if device is None:
    print(sd.query_devices())
    exit()


samplerate = 44100
channels = 1
blocksize = 1024

class MonoSoundSource:
    def __init__(self):
        self.pos = 0

    def read(self, size):
        t = (self.pos + np.arange(size)) / samplerate
        self.pos += size
        return np.sin(2 * np.pi * 220 * t.reshape(-1, 1))

source = MonoSoundSource()

def callback(outdata, frames, time, status):
    if status:
        print(status, file=sys.stderr)

    outdata[:] = struct.pack(f"{frames}f", *(source.read(frames)))

with sd.RawOutputStream(samplerate=samplerate, blocksize=blocksize, device=device, channels=channels, dtype="float32", callback=callback):
    input()


