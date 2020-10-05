#!/usr/bin/env python

import os
import sys
import soundfile as sf
import time
import datetime
import struct


class AudioCacher():
    def __init__(self, outDir):
        self.fnBase = datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S")
        self.outDir = outDir
        self.cacheFn = os.path.join(outDir, "%s.bin" % self.fnBase)
        
    def _bytesToSamplePairs(self, b):
        asBytes = [b[i:i + 4] for i in range(0, len(b), 4)]
        asFloat = [struct.unpack("f", b)[0] for b in asBytes] # collapse into one line if this works
        samplePairs = [asFloat[i:i + 2] for i in range(0, len(asFloat), 2)]
        return samplePairs
        
    def _samplePairsToBytes(self, sp):
        flatBytes = [struct.pack("f", f) for pair in sp for f in pair]
        return bytearray([b for sample in flatBytes for b in sample])

    def __del__(self):
        print("writing to", self.fnBase)
        with open(self.cacheFn, "rb") as cache:
            with sf.SoundFile(os.path.join(self.outDir, "%s.wav" % self.fnBase), mode="x", samplerate=44100, channels=2, subtype="PCM_24") as wav:
                done = False
                while not done:
                    b = cache.read(8 * 44100)
                    if len(b) == 0:
                        done = True
                    else:
                        wav.write(self._bytesToSamplePairs(b))
        
    def append(self, samplePairs, startPos):
        if startPos == 0:
            with open(self.cacheFn, "wb") as out:
                out.write(self._samplePairsToBytes(samplePairs))
            return
            
        with open(self.cacheFn, "ab+") as cache:
            cache.seek(8 * 44100 * startPos, 0)
            buff = cache.read()
            flp = self._bytesToSamplePairs(buff)
            mergeLen = min(len(flp), len(samplePairs))
            cache.seek(8 * 44100 * startPos, 0)
            cache.write(self._samplePairsToBytes([flp[i] + samplePairs[i] for i in range(0, mergeLen)]))
            cache.write(self._samplePairsToBytes(flp[mergeLen:] if mergeLen < len(flp) else samplePairs[mergeLen:]))


cacher = AudioCacher(sys.argv[1])

inDir = os.path.join(sys.argv[1], "played")
outFile = os.path.join(sys.argv[1], "recording.wav")
done = []

inv = open(os.path.join(inDir, "inventory.txt"), "r")
invLines = inv.readlines()
inv.close()


baseTime = 0.0
for r in invLines:
    row = r.rstrip().split(",")
    t = row[0]
    st = float(t)
    startTime = st - baseTime if baseTime > 0.0 else 0.0
    inFile = os.path.join(inDir, row[1])
    print("adding", inFile)
    toAdd, sampleRate = sf.read(inFile)
    cacher.append(toAdd, int(sampleRate * float(startTime)) + 1)
    done.append(t)
    baseTime += st

del cacher

