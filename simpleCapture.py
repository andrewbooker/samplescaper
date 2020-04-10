#!/usr/bin/env python

import queue
import sounddevice as sd

import datetime
import time
import sys
import os
import keyboard
import threading
from multiprocessing import Value

from utils.LoopableSample import LoopableSample

def usableAudioDevices():
    usable = {}

    devs = sd.query_devices()
    for d in range(len(devs)):
        dev = devs[d]
        if "USB" in dev["name"] and dev["default_samplerate"] == 44100:
            usable[d] = dev["name"]

    return usable

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
        self.dirOut = "%s/%s" % (dirOut, datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S"))
        os.makedirs(self.dirOut)

    def start(self, sampleNumber, shouldStop, shouldRecordClip):
        stream = sd.InputStream(samplerate=44100.0, device=self.device, channels=1, callback=self.buffer.make(), blocksize=512)
        out = None
        fn = 0
        while not shouldStop.is_set():
            if out is None and shouldRecordClip.is_set():
                stream.start()
                out = LoopableSample()

            if out is not None:
                if not shouldRecordClip.is_set():
                    stream.stop()
                    out.addBuffer(self.buffer.q.get())

                    outDir = "%s/%d" % (self.dirOut, sampleNumber.value)
                    if not os.path.exists(outDir):
                        os.makedirs(outDir)
                    fqfn = "%s/sample_%s.wav" % (outDir, fn)
                    print("Writing to %s" % fqfn)
                    out.create(fqfn)
                    out = None
                    fn += 1
                    print("ready")
                else:
                    out.addBuffer(self.buffer.q.get())
            else:
                time.sleep(0.1)

        if out is not None:
            stream.stop()

if len(sys.argv) < 2:
    print("please supply output directory (will be created if necessary)")
    exit()

outDir = sys.argv[1]
buffer = Buffer()
if not os.path.exists(outDir):
    os.makedirs(outDir)
sampleNumber = Value('i', int(sys.argv[2]) if len(sys.argv) > 2 else 48)
shouldStop = threading.Event()
shouldRecordClip = threading.Event()

devices = usableAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using %s" % devices[audioDevice])

recording = RecordSamples(audioDevice, outDir, buffer)
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
        shouldRecordClip.clear()

keyboard.on_press_key("q", stopCapture, suppress = True)
keyboard.on_press_key("n", incrementNumber, suppress = True)
keyboard.on_press_key("s", toggleRecord, suppress = True)

while not shouldStop.is_set():
    time.sleep(1)

print("done")
