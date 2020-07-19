#!/usr/bin/env python

import datetime
import time
import queue
import sounddevice as sd
import os
import sys

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
from utils.LoopableSample import LoopableSample

sampleRate = 44100.0


class Buffer():
    def __init__(self):
        self.q = queue.Queue()

    def make(self):
        def handle(indata, frames, time, status):
            self.q.put(indata.copy())
        return handle


class Consumer():
    def __init__(self, outDir):
        self.out = None
        self.done = False
        self.sampleLength = 0
        self.outDir = outDir

    def addBuffer(self, b):
        if self.out is None:
            self.out = LoopableSample()

        self.out.addBuffer(b)

        if self.sampleLength > 3.0:
            fn = "%s.wav" % datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S")
            fqfn = os.path.join(self.outDir, fn)
            print("Writing to %s" % fqfn)
            self.out.create(fqfn)
            self.out = None
            self.sampleLength = 0
            print("ready")

        #if zero but too short    
            #abandon : self.out = None
        
        self.sampleLength += (len(b) / sampleRate)
    

class Recorder():
    def __init__(self, device, dirOut):
        self.device = device
        self.buffer = Buffer()
        self.dirOut = dirOut
        self.stream = None

    def __del__(self):
        if self.stream is not None:
            self.stream.stop()

    def start(self):
        self.stream = sd.InputStream(samplerate=sampleRate, device=self.device, channels=1, callback=self.buffer.make(), blocksize=512)
        
        self.stream.start()
        consumer = Consumer(self.dirOut)
        while True:
            consumer.addBuffer(self.buffer.q.get())


devices = UsbAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using", devices[audioDevice])
outDir = sys.argv[1]


recorder = Recorder(audioDevice, outDir)
recorder.start()

