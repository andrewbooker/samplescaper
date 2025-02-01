#!/usr/bin/env python

import sys
import sounddevice as sd
import soundfile as sf
import struct
import numpy as np
import random
import os
import time
import threading
from datetime import datetime
from pathlib import Path
from enum import Enum
from http.server import HTTPServer, BaseHTTPRequestHandler
import json


device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 0.3
inDir = sys.argv[3] if len(sys.argv) > 3 else None
channels = int(sys.argv[4]) if len(sys.argv) > 4 else 3

if device is None:
    print(sd.query_devices())
    exit()


samplerate = 44100
blocksize = 16384
maxLeadInSecs = 5
vols = [
    0.0,
    0.0,
    1.0,
    0.0,
    0.0,
    0.0,
    0.0,
    0.0
]
allOn = [1.0] * 8
vols = allOn

class MonoSoundSource:
    def __init__(self):
        self.pos = 0

    def advance(self, size):
        self.pos += size
        pass

    def signal_to_stop(self):
        pass


class MonoSineSource(MonoSoundSource):
    def __init__(self, freq):
        super(MonoSineSource, self).__init__()
        self.freq = freq

    def read(self, size):
        t = (self.pos + np.arange(size)) / samplerate
        super().advance(size)
        return level * np.sin(2 * np.pi * self.freq * t.reshape(-1, 1))

    def isFinished(self):
        return False


class Loading(Enum):
    NotSetUp = 0
    NotStarted = 1
    Started = 2
    AwaitingFinish = 3
    Finished = 4


class AudioFileLoader:
    def __init__(self, inDir):
        self.inDir = inDir
        parent_dir = os.path.dirname(inDir)
        self.currently_loading_for = None
        self.loading = None
        self.done_dir = os.path.join(parent_dir, datetime.now().strftime("%Y%m%d_%H%M%S"))
        print("Playing files in", inDir)
        print("Moving played files to", self.done_dir)
        Path(self.done_dir).mkdir()
        self.load_state = Loading.NotSetUp

    def getFile(self):
        rawFiles = os.listdir(self.inDir)
        if len(rawFiles) == 0:
            return
        print(self.currently_loading_for.me, "choosing from", len(rawFiles), "files")
        random.shuffle(rawFiles)
        selected = rawFiles[0]
        leadIn = maxLeadInSecs * random.random()
        file_to_open = os.path.join(self.inDir, selected)
        data, _ = sf.read(file_to_open)
        self.currently_loading_for.fileBuffer = [0.0] * int(leadIn * samplerate)
        self.currently_loading_for.fileBuffer.extend([(vols[self.currently_loading_for.me] * level * d) for d in data])
        os.rename(file_to_open, os.path.join(self.done_dir, selected))
        self.load_state = Loading.AwaitingFinish

    def request_file_for(self, source):
        if self.currently_loading_for is not None and self.currently_loading_for.me != source.me:
            return False

        match self.load_state:
            case Loading.Finished:
                self.loading = None
                self.currently_loading_for.is_ready = True
                self.currently_loading_for = None
                self.load_state = Loading.NotSetUp
                return True
            case Loading.AwaitingFinish:
                self.loading.join()
                self.load_state = Loading.Finished
                return False
            case Loading.NotStarted:
                self.loading.start()
                self.load_state = Loading.Started
                return False
            case Loading.NotSetUp:
                self.currently_loading_for = source
                self.loading = threading.Thread(target=self.getFile, daemon=True)
                self.load_state = Loading.NotStarted
                return False
            case _:
                return False


loader = AudioFileLoader(inDir)


class MonoWavSource(MonoSoundSource):
    def __init__(self, loader, idx):
        super(MonoWavSource, self).__init__()
        self.loader = loader
        self.fileBuffer = None
        self.start = time.time()
        self.me = idx
        self.is_ready = False
        self.should_stop = False
        self.has_stopped = False

    def isFinished(self):
        return self.has_stopped

    def signal_to_stop(self):
        self.should_stop = True

    def signal_to_start(self):
        self.should_stop = False
        self.has_stopped = False

    def read(self, size):
        if not self.is_ready:
            empty = [0.0] * size
            if self.should_stop:
                self.has_stopped = True
                return empty

            self.loader.request_file_for(self)
            return empty

        ret = self.fileBuffer[self.pos:self.pos + size]
        available = len(ret)
        super().advance(size)
        if available == size:
            return ret
        print(self.me, "reached the end")
        self.is_ready = False
        self.pos = 0
        empty = [0.0] * size
        empty[:available] = ret
        return empty


class Player():
    def __init__(self):
        self.should_stop = threading.Event()
        self.loop_plater = None
        self.sources = []

        if inDir is not None:
            self.sources.extend([
                MonoWavSource(loader, i) for i in range(channels)
            ])
        else:
            self.sources.extend([
                MonoSineSource(220),
                MonoSineSource(441)
            ])

        self.playing = len(self.sources)
        print(f"playing {self.playing} sources")


    def stop(self):
        print("received signal to stop")
        self.should_stop.set()
        for s in self.sources:
            s.signal_to_stop()

        while self.loop_player.is_alive():
            time.sleep(0.1)
        self.loop_player.join()
        print("finished")


    def play(self):
        def callback(outdata, frames, time, status):
            if status:
                print(status, file=sys.stderr)

            block = np.dstack([s.read(frames) for s in self.sources]).flatten()
            outdata[:] = struct.pack(f"{self.playing * frames}f", *block)

        def loop():
            print("Playing loop running")
            with sd.RawOutputStream(samplerate=samplerate, blocksize=blocksize, device=device, channels=self.playing, dtype="float32", callback=callback):
                done = False
                while not done:
                    if self.should_stop.is_set():
                        if all([s.isFinished() for s in self.sources]):
                            done = True
                    time.sleep(1)
                print("Playing loop terminated")
                return

        self.loop_player = threading.Thread(target=loop, args=(), daemon=False)
        self.loop_player.start()


player = Player()

class Controller(BaseHTTPRequestHandler):
    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def do_OPTIONS(self):
        self._standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, POST")
        self.send_header("Access-Control-Allow-Headers", "Current-Time")
        self.send_header("Access-Control-Allow-Headers", "Tonic")
        self.send_header("Access-Control-Allow-Headers", "Mode")
        self.end_headers()

    def do_POST(self):
        getattr(self, "_%s" % self.path[1:])()
        self._standardResponse()
        self.wfile.write(json.dumps({}).encode("utf-8"))
        self.end_headers()

    def _play(self):
        player.play()

    def _stop(self):
        player.stop()

    def _pause(self):
        player.pause()


HTTPServer(("0.0.0.0", 9966), Controller).serve_forever()

