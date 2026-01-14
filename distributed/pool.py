#!/usr/bin/env python

import datetime
import time
import queue
import sounddevice as sd
import os
import sys
import shutil
import soundfile as sf

localParentDir = os.path.dirname(os.getcwd())
parentDir = os.path.dirname(localParentDir)
sys.path.append(localParentDir)
sys.path.append(parentDir)
def checkImport(lib):
    if not os.path.exists(os.path.join(parentDir, lib)):
        print("%s library not found." % lib)
        print("please clone github.com/andrewbooker/%s.git into %s" % (lib, parentDir))
        exit()

checkImport("mediautils")
from mediautils.audiodevices import UsbAudioDevices
from utils.LevelMonitor import LevelMonitor

SAMPLE_RATE = 44100.0
MINIMAL_LEVEL = 5.0
MAX_LIVE_POOL_SIZE = 10


class SampleCatcher():
    def __init__(self):
        self.data = []

    def addBuffer(self, buffer):
        self.data.extend(buffer)


class Buffer():
    def __init__(self):
        self.q = queue.Queue()

    def make(self):
        def handle(indata, frames, time, status):
            self.q.put(indata.copy())
        return handle


class PoolFeeder():
    def __init__(self, outDir, monitor):
        self.outDir = outDir
        self.monitor = monitor
        self.reset()
        self.out = None

    def reset(self):
        self.out = None
        self.sampleLength = 0
        self.monitor.setRecording(0)

    def addBuffer(self, b):
        dropped = sum([abs(v) for v in b]) < MINIMAL_LEVEL

        if self.sampleLength > 2.0:
            self.monitor.setRecording(2)
            self.monitor.setMessage("recording")
        elif dropped:
            if self.out is not None:
                self.reset()
            return

        if self.out is None:
            self.out = SampleCatcher()
            self.monitor.setRecording(1)

        if dropped or self.sampleLength > 5.0:
            fd = datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S")
            fqfn = os.path.join(self.outDir, f"{fd}.wav")
            sf.write(fqfn, self.out.data, 44100)
            self.monitor.setMessage(f"Written {len(self.out.data)} samples to to {fqfn}")
            self.reset()
            return

        self.out.addBuffer(b)
        self.sampleLength += (len(b) / SAMPLE_RATE)


class Recorder():
    def __init__(self, device, consumers):
        self.device = device
        self.buffer = Buffer()
        self.consumers = consumers
        self.stream = None

    def __del__(self):
        if self.stream is not None:
            self.stream.stop()
            print("audio stream stopped")

    def start(self, shouldStop):
        self.stream = sd.InputStream(samplerate=SAMPLE_RATE, device=self.device, channels=1, callback=self.buffer.make(), blocksize=512)

        self.stream.start()
        while not shouldStop.is_set():
            b = self.buffer.q.get()
            [c.addBuffer(b) for c in self.consumers]


class PoolMaintainer():
    def _create(self, poolDir, sub):
        p = os.path.join(poolDir, sub)
        if not os.path.exists(p):
            os.makedirs(p)
        return p

    def __init__(self, poolDir):
        self.outDir = self._create(poolDir, "live")
        self.oldDir = self._create(poolDir, "dead")
        self.maxSize = MAX_LIVE_POOL_SIZE

    def clear(self):
        self.maxSize = 0

    def setSize(self, s):
        self.maxSize = s

    def start(self, shouldStop):
        i = 0
        while not shouldStop.is_set():
            if i > 10:
                files = [os.path.join(self.outDir, f) for f in os.listdir(self.outDir)]
                sortedFiles = sorted(files, key=lambda f: os.path.getmtime(f))
                number = len(sortedFiles)
                if number > self.maxSize:
                    for f in sortedFiles[:(number - self.maxSize)]:
                        shutil.move(f, self.oldDir)
                    if self.maxSize == 0:
                        self.maxSize = MAX_LIVE_POOL_SIZE
                i = 0

            time.sleep(0.2)
            i += 1

devices = UsbAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using", devices[audioDevice])


poolDir = sys.argv[1]
maintainer = PoolMaintainer(poolDir)
level = LevelMonitor()
feeder = PoolFeeder(maintainer.outDir, level)

import threading
import readchar

shouldStop = threading.Event()
recorder = Recorder(audioDevice, [feeder, level])

threads = []
threads.append(threading.Thread(target=level.start, args=(shouldStop,), daemon=True))
threads.append(threading.Thread(target=recorder.start, args=(shouldStop,), daemon=True))
threads.append(threading.Thread(target=maintainer.start, args=(shouldStop,), daemon=True))

[t.start() for t in threads]
done = False

import readchar
print("Started. Press 'q' to exit, 'c' to clear the pool, 'm' to max pool size")
while not done:
    c = readchar.readchar()
    if c == "q":
        shouldStop.set()
        done = True
    if c == "c":
        level.setMessage("clearing the pool")
        maintainer.clear()
    if c == "m":
        maintainer.setSize(MAX_LIVE_POOL_SIZE)
        level.setMessage(f"pool size set to {MAX_LIVE_POOL_SIZE}")
    if ord(c) >= 48 and ord(c) < 58:
        maintainer.setSize(int(c))
        level.setMessage(f"pool size set to {c}")


[t.join() for t in threads]

