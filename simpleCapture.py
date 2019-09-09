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
from multiprocessing import Value



# audio capture

class Buffer():
    def __init__(self):
        self.q = queue.Queue()

    def make(self):
        def handle(indata, frames, time, status):
            self.q.put(indata.copy())
        return handle

class RecordSamples():
    def __init__(self, device, dirOut, buffer):
        self.device = device
        self.buffer = buffer
        self.dirOut = "%s/%s" % (dirOut, datetime.datetime.fromtimestamp(now).strftime("%Y-%m-%d_%H%M%S"))
        os.makedirs(self.dirOut)

    def start(self, sampleNumber, shouldStop, shouldRecordClip):
        stream = sd.InputStream(samplerate=44100.0, device=self.device, channels=1, callback=self.buffer.make(), blocksize=512)
        out = None
        fn = 0
        while not shouldStop.is_set():
            if out is None and shouldRecordClip.is_set():
                outDir = "%s/%d" % (self.dirOut, sampleNumber.value)
                if not os.path.exists(outDir):
                    os.makedirs(outDir)
                stream.start()
                fqfn = "%s/sample_%s.wav" % (outDir, fn)
                print("Opening %s" % fqfn)
                out = sf.SoundFile(fqfn, mode="x", samplerate=44100, channels=1, subtype="PCM_16")

            if out is not None:
                if not shouldRecordClip.is_set():
                    out.close()
                    out = None
                    stream.stop()
                    fn += 1
                else:
                    out.write(self.buffer.q.get())
            else:
                time.sleep(0.1)

        if out is not None:
            out.close()
            stream.stop()



buffer = Buffer()

sampleNumber = Value('i', 48)
now = time.time()
outDir = sys.argv[1]
if not os.path.exists(outDir):
    os.makedirs(outDir)

shouldStop = threading.Event()
shouldRecordClip = threading.Event()

recording = RecordSamples(0, outDir, buffer) # "Microphone (Blue Snowball ), MME"
recordThread = threading.Thread(target = recording.start, args = (sampleNumber, shouldStop, shouldRecordClip), daemon = True)

print("ready to record to %s" % outDir)
recordThread.start()

def incrementNumber(e):
    with sampleNumber.get_lock():
        sampleNumber.value += 1
    print("next sample number %d" % sampleNumber.value)

def stopCapture(e):
    print("stopping...")
    shouldStop.set()
    recordThread.join()

def toggleRecord(e):
    if not shouldRecordClip.is_set():
        print("starting at queue size %d..." % buffer.q.qsize())
        shouldRecordClip.set()
    else:
        print("stopping at queue size %d..." % buffer.q.qsize())
        time.sleep(0.5)
        shouldRecordClip.clear()

keyboard.on_press_key("q", stopCapture, suppress = True)
keyboard.on_press_key("n", incrementNumber, suppress = True)
keyboard.on_press_key(" ", toggleRecord, suppress = True)

while not shouldStop.is_set():
    time.sleep(1)

print("done")
