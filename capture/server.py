#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import sounddevice as sd
import os
import sys
parentDir = os.path.dirname(os.getcwd())
sys.path.append(parentDir)
from capture.sampleRecording import Buffer, SampleRecorder
import threading
from multiprocessing import Value

#usage
#./server.py ~/Music/samples

class Controller():
    def __init__(self, audioDevice, outDir):
        self.buffer = Buffer()
        self.sampleNumber = Value('i', 0)
        self.shouldStop = threading.Event()
        self.shouldRecordClip = threading.Event()

        self.recording = SampleRecorder(audioDevice, outDir, self.buffer)
        self.recordThread = threading.Thread(target = self.recording.start, args = (self.sampleNumber, self.shouldStop, self.shouldRecordClip), daemon = True)

        print("ready to record to %s" % outDir)
        self.recordThread.start()

    def __del__(self):
        print("stopping recording")
        self.shouldRecordClip.clear()
        print("stopping stream")
        self.shouldStop.set()
        print("stopping thread")
        self.recordThread.join()
        print("stopping recording server controller...")

    def _setNumber(self, n):
        with self.sampleNumber.get_lock():
            self.sampleNumber.value = n
        print("next sample number %d" % self.sampleNumber.value)

    def startRecording(self, note):
        self._setNumber(note)
        self.shouldRecordClip.set()
        print("starting at queue size %d..." % self.buffer.q.qsize())

    def stopRecording(self):
        self.shouldRecordClip.clear()
        print("stopped at queue size %d..." % self.buffer.q.qsize())


# 7 = USB audio device
controller = Controller(7, sys.argv[1])

class SampleCaptureServer(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()
        resp = {
            "is_recording": controller.shouldRecordClip.is_set()
        }
        self.wfile.write(json.dumps(resp).encode("utf-8"))

    def do_POST(self):
        body = self.rfile.read(int(self.headers['content-length']))
        data = json.loads(body)
        if data["action"] == "on":
            controller.startRecording(int(data["note"]))
        else:
            controller.stopRecording()
        self.send_response(200)

def startServer():
    HTTPServer(("0.0.0.0", 9009), SampleCaptureServer).serve_forever()


startServer()