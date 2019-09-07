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
        with sd.InputStream(samplerate=44100.0, device=self.dev, channels=1, callback=self.cb.make()):
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
         
buffer = Buffer()

now = time.time()    
outDir = sys.argv[1]
if not os.path.exists(outDir):
    os.makedirs(outDir)
    
shouldStop = threading.Event()

recording = RecordAudio(outDir, buffer)
capture = CaptureAudio(1, buffer)

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
