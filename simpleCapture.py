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


from utils.sampleRecording import *


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
