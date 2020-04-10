#!/usr/bin/env python

import datetime
import time
import os
import queue
import sounddevice as sd
from utils.LoopableSample import LoopableSample

def usableAudioDevices():
    usable = {}

    devs = sd.query_devices()
    for d in range(len(devs)):
        dev = devs[d]
        if "USB" in dev["name"] and dev["default_samplerate"] == 44100:
            usable[d] = dev["name"]

    return usable


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
