#!/usr/bin/env python

import datetime
import time
import queue
import sounddevice as sd
import sys
import os
parentDir = os.path.dirname(os.getcwd())
sys.path.append(parentDir)
from capture.LoopableSample import LoopableSample


class Buffer():
    def __init__(self):
        self.q = queue.Queue()

    def make(self):
        def handle(indata, frames, time, status):
            self.q.put(indata.copy())
        return handle

class SampleRecorder():
    def __init__(self, device, dirOut, sampleBuffer, subfoldersPerNote):
        self.device = device
        self.buffer = sampleBuffer
        self.subfoldersPerNote = subfoldersPerNote
        self.dirOut = dirOut
        self.fileNameStem = datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S")
        self.isWriting = False
        self.shouldWrite = True

    def writeFileFrom(self, out, sampleNumber, fn):
        l = out.length() / 44100.0
        print("buffer length %.02fs" % l)
        if l < 1.0:
            print("too short to bother with. Abandoning.")
            return

        if self.shouldWrite:
            self.isWriting = True
            print("writing %03d" % fn)
            out.addBuffer(self.buffer.q.get())

            outDir = [self.dirOut]
            fName = []
            if self.subfoldersPerNote:
                outDir.append(self.fileNameStem)
                outDir.append(str(sampleNumber.value))
                d = os.path.join(*outDir)
                if not os.path.exists(d):
                    os.makedirs(d)
            else:
                fName.append(str(sampleNumber.value))
                fName.append(self.fileNameStem)

            fName.append("%03d.wav" % fn)
            outDir.append("_".join(fName))

            fqfn = os.path.join(*outDir)
            print("Writing to %s" % fqfn)
            out.create(fqfn)
        else:
            print("should not write")

        self.isWriting = False

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
                    self.writeFileFrom(out, sampleNumber, fn)
                    fn += 1
                    out = None
                    print("ready")
                else:
                    out.addBuffer(self.buffer.q.get())
            else:
                time.sleep(0.1)

        if out is not None:
            stream.stop()
            
import threading
from multiprocessing import Value
            
class Controller():
    def __init__(self, sampleNumber, audioDevice, outDir):
        self.buffer = Buffer()
        self.sampleNumber = Value('i', sampleNumber)
        self.shouldStop = threading.Event()
        self.shouldRecordClip = threading.Event()
        
        self.recording = SampleRecorder(audioDevice, outDir, self.buffer, True)
        self.recordThread = threading.Thread(target = self.recording.start, args = (self.sampleNumber, self.shouldStop, self.shouldRecordClip), daemon = True)

        print("ready to record to %s" % outDir)
        self.recordThread.start()
        
    def setNumber(self, n):
        with self.sampleNumber.get_lock():
            self.sampleNumber.value = n
        print("next sample number %d" % self.sampleNumber.value)
        
    def incrementNumber(self, e):
        self.setNumber(self.sampleNumber.value + 1)

    def stopCapture(self, e):
        print("stopping...")
        self.shouldStop.set()
        self.recordThread.join()

    def toggleRecord(self, e):
        if not self.shouldRecordClip.is_set():
            print("starting at queue size %d..." % self.buffer.q.qsize())
            self.shouldRecordClip.set()
        else:
            print("stopping at queue size %d..." % self.buffer.q.qsize())
            self.shouldRecordClip.clear()
