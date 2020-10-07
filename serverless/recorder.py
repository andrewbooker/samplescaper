#!/usr/bin/env python

import os
import sys
import soundfile as sf
import time
import datetime
import struct

BYTES_PER_SAMPLE=8


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

    def dump(self):
        start = time.time()
        print("writing to", self.fnBase)
        with open(self.cacheFn, "rb") as cache:
            with sf.SoundFile(os.path.join(self.outDir, "%s.wav" % self.fnBase), mode="x", samplerate=44100, channels=2, subtype="PCM_24") as wav:
                done = False
                while not done:
                    b = cache.read(BYTES_PER_SAMPLE * 44100)
                    if len(b) == 0:
                        done = True
                    else:
                        wav.write(self._bytesToSamplePairs(b))
        print("done in", time.time() - start)

    def append(self, samplePairs, startPos):
        startBytes = BYTES_PER_SAMPLE * startPos
        catchup = startBytes
        if os.path.exists(self.cacheFn):
            catchup -= os.stat(self.cacheFn).st_size

        with open(self.cacheFn, "ab+") as cache:
            if catchup > 0:
                cache.write(bytearray([0] * catchup))
                cache.write(self._samplePairsToBytes(samplePairs))
                cache.flush()
            else:
                cache.seek(startBytes)
                buff = cache.peek()
                flp = self._bytesToSamplePairs(buff)
                mergeLen = min(len(flp), len(samplePairs))
                cache.write(self._samplePairsToBytes([flp[i] + samplePairs[i] for i in range(0, mergeLen)]))
                cache.write(self._samplePairsToBytes(flp[mergeLen:] if mergeLen < len(flp) else samplePairs[mergeLen:]))
                cache.flush()

def pollInventory(inFile, cacher, shouldStop):
    inDir = sys.argv[1]
    done = []
    while not shouldStop.is_set():
        inv = open(inFile, "r")
        invLines = inv.readlines()
        inv.close()

        for r in invLines:
            row = r.rstrip().split(",")
            t = row[0]
            if t not in done:
                st = float(t)
                f = row[1]
                df = os.path.join(inDir, "looped")
                if not os.path.exists(os.path.join(df, f)):
                    df = os.path.join(inDir, "played")

                inFile = os.path.join(df, f)
                sys.stdout.write("adding %s %s\n\r" % (inFile, t))
                toAdd, sampleRate = sf.read(inFile)
                cacher.append(toAdd, int((sampleRate * float(st)) + 0.5))
                done.append(t)
            else:
                sys.stdout.write("done %s\n\r")

        time.sleep(1)
        shouldStop.set()

cacher = AudioCacher(sys.argv[1])

import threading
shouldStop = threading.Event()

inFile = os.path.join(sys.argv[1], "inventory.txt")

print("Starting... press 'q' to exit")
t = threading.Thread(target=pollInventory, args=(inFile, cacher, shouldStop), daemon=True)
t.start()

import readchar
while not shouldStop.is_set():
    c = readchar.readchar()
    if c == "q":
        shouldStop.set()

t.join()
cacher.dump()
