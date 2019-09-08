#!/usr/bin/env python

import queue
import sounddevice as sd
import soundfile as sf
import numpy

import datetime
import time
import sys
import os
import keyboard
import threading

# utils

import math


def any(a, ommitting = []):
    f = a[randint(0, len(a) - 1)]
    if f in ommitting:
        return any(a, ommitting)
    return f


class MovingAvg():
    def __init__(self, size):
        self.size = size
        self.clear()

    def clear(self):
        self.values = []
        self.avg = 0.0

    def add(self, v):
        self.avg += (v * 1.0 / self.size)
        self.values.append(v)
        if (len(self.values) > self.size):
            p = self.values.pop(0)
            self.avg -= (p * 1.0 / self.size)

    def first(self):
        return self.values[0]

    def value(self):
        return self.avg if (len(self.values) == self.size) else (self.avg * self.size / len(self.values))


class AbsMovingAvg(MovingAvg):
    def __init__(self, size):
        MovingAvg.__init__(self, size)

    def add(self, v):
        self.avg += (abs(v) * 1.0 / self.size)
        self.values.append(v)
        if (len(self.values) > self.size):
            p = self.values.pop(0)
            self.avg -= (abs(p) * 1.0 / self.size)

# audio capture

class Buffer():
    def __init__(self):
        self.q = queue.Queue()

    def make(self):
        def handle(indata, frames, time, status):
            self.q.put(indata.copy())
        return handle

class CaptureAudio():
    def __init__(self, dev, cb):
        self.dev = dev
        self.cb = cb

    def start(self, shouldStop):
        print("starting audio capture on %s" % self.dev)
        with sd.InputStream(samplerate=44100.0, device=self.dev, channels=1, callback=self.cb.make(), blocksize=512):
            while not shouldStop.is_set():
                time.sleep(1)

            print("stopping audio capture on %s" % self.dev)

class RecordAudio():
    def __init__(self, loc, buffer):
        self.loc = loc
        self.buffer = buffer

    def start(self, shouldStop):
        fn = datetime.datetime.fromtimestamp(now).strftime("%Y-%m-%d_%H%M%S")
        with sf.SoundFile("%s/%s.wav" % (self.loc, fn), mode="x", samplerate=44100, channels=1, subtype="PCM_16") as file:
            while not shouldStop.is_set():
                file.write(self.buffer.q.get())

class RecordSamples():
    def __init__(self, dirOut, buffer, gain):
        self.dirOut = "%s/%s" % (dirOut, datetime.datetime.fromtimestamp(now).strftime("%Y-%m-%d_%H%M%S"))
        os.makedirs(self.dirOut)
        self.gain = gain
        self.buffer = buffer
        self.out = None
        self.state = 0
        self.movingAvg5 = AbsMovingAvg(5)
        self.movingAvg30 = AbsMovingAvg(30)
        self.fn = 0
        self.lastStart = 0
        self.i = 0

    def _readOneSample(self, v):
        self.movingAvg5.add(v)
        self.movingAvg30.add(v)

        if (self.state == 2):
            self.movingAvg5.clear()
            self.movingAvg30.clear()
            self.state = 0

        if (self.state == 1 and ((self.i - self.lastStart) > 2 * 44100) and self.movingAvg30.avg < 0.003):
            print("ending sample at %d at %d" % (self.fn, self.i))
            self.state = 2
            self.out.close()
            self.out = None
            self.fn += 1

        if (self.state == 0 and abs(v - self.movingAvg5.avg) > 0.25):
            print("beginning sample %d at %d" % (self.fn, self.i))
            self.out = sf.SoundFile("%s/sample%03d.wav" % (self.dirOut, self.fn), mode="x", samplerate=44100, channels=1, subtype="PCM_16")
            self.state = 1
            self.lastStart = self.i

        if (self.state == 1):
            self.out.write(self.movingAvg5.first())

    def start(self, shouldStop):
        while not shouldStop.is_set():
            #print("queue length %d" % self.buffer.q.qsize())
            data = self.buffer.q.get()
            for d in data:
                self._readOneSample(d * self.gain)
                self.i += 1


buffer = Buffer()

now = time.time()
outDir = sys.argv[1]
if not os.path.exists(outDir):
    os.makedirs(outDir)

shouldStop = threading.Event()

recording = RecordSamples(outDir, buffer, 10)
capture = CaptureAudio("Microphone (Blue Snowball ), MME", buffer)

captureThread = threading.Thread(target = capture.start, args = (shouldStop,), daemon = True)
recordThread = threading.Thread(target = recording.start, args = (shouldStop,), daemon = True)

print("starting recording to %s" % outDir)
recordThread.start()
captureThread.start()

keyboard.wait("q")
print ("stopping...")
shouldStop.set()
recordThread.join()
captureThread.join()
print("done")
