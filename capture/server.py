#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import sounddevice as sd
import os
import sys
import time
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
        self.startCmdThread = None
        self.stopCmdThread = None
        self.recordingStarted = None

    def __del__(self):
        print("stopping stream")
        self.shouldStop.set()
        print("stopping thread")
        self.recordThread.join()
        print("stopping recording server controller...")

    def _setNumber(self, n):
        with self.sampleNumber.get_lock():
            self.sampleNumber.value = n
        print("next sample number %d" % self.sampleNumber.value)

    def _recordingStart(self, note):
        if self.stopCmdThread:
            self.stopCmdThread.join()
        while self.recording.isWriting:
            time.sleep(0.1)
        print("starting recording")
        self._setNumber(note)
        self.shouldRecordClip.set()
        self.recording.shouldWrite = True
        self.recordingStarted = time.time()

    def _recordingStop(self):
        self.startCmdThread.join()
        self.startCmdThread = None
        dt = time.time() - self.recordingStarted
        if dt < 1.0:
            print("abandoning recording after %.2fs" % dt)
            self.recording.shouldWrite = False
            self.shouldRecordClip.clear()
            return

        if not self.shouldRecordClip.is_set():
            print("received stop command but recording was not started")
            return

        print("stopping recording after %.2fs" % dt)
        self.shouldRecordClip.clear()

    def startRecording(self, note):
        if self.startCmdThread is not None:
            print("ignoring repeat start command")
            return
        self.startCmdThread = threading.Thread(target=self._recordingStart, args=(note,), daemon=True)
        self.startCmdThread.start()

    def stopRecording(self):
        if self.startCmdThread is None:
            print("received stop command before start command")
            return

        self.stopCmdThread = threading.Thread(target=self._recordingStop, args=(), daemon=True)
        self.stopCmdThread.start()


controller = Controller(2, sys.argv[1])

class SampleCaptureServer(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()
        resp = {
            "is_recording": controller.shouldRecordClip.is_set(),
            "is_writing": controller.recording.isWriting
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
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()
        self.wfile.write(json.dumps({}).encode("utf-8"))

def startServer():
    HTTPServer(("0.0.0.0", 9009), SampleCaptureServer).serve_forever()


startServer()
