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
import logging



device = int(sys.argv[1]) if len(sys.argv) > 1 else None
level = float(sys.argv[2]) if len(sys.argv) > 2 else 0.3
inDir = sys.argv[3] if len(sys.argv) > 3 else None
channels = int(sys.argv[4]) if len(sys.argv) > 4 else 3

if device is None:
    print(sd.query_devices())
    exit()


logger = logging.getLogger("SingleUnitRandomatones")
logger.setLevel(logging.INFO)
fh = logging.FileHandler("/var/log/samplescaper/player.log")
fh.setLevel(logging.INFO)
logger.addHandler(fh)


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
        t = (self.pos + np.arange(size)) / 44100
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
    NoFiles = 5


class AudioFileLoader:
    def __init__(self):
        self.currently_loading_for = None
        self.load_state = Loading.NotSetUp
        self.loading = None

    def getFile(self):
        pass

    def supply(self, data, pad = []):
        self.currently_loading_for.fileBuffer = pad
        self.currently_loading_for.fileBuffer.extend([(level * d) for d in data])
        self.load_state = Loading.AwaitingFinish

    def request_file_for(self, source):
        if self.currently_loading_for is not None and self.currently_loading_for.me != source.me:
            return

        if self.load_state == Loading.Finished:
            self.loading = None
            self.currently_loading_for.is_ready = True
            self.currently_loading_for = None
            self.load_state = Loading.NotSetUp
            return
        if self.load_state == Loading.AwaitingFinish:
            self.loading.join()
            self.load_state = Loading.Finished
            return
        if self.load_state == Loading.NoFiles:
            self.loading.join()
            del self.loading
            self.loading = threading.Thread(target=self.getFile, daemon=True)
            self.load_state = Loading.NotStarted
            return
        if self.load_state == Loading.NotStarted:
            self.loading.start()
            self.load_state = Loading.Started
            return
        if self.load_state == Loading.NotSetUp:
            self.currently_loading_for = source
            self.loading = threading.Thread(target=self.getFile, daemon=True)
            self.load_state = Loading.NotStarted


import requests
import io
class HttpFileFetcher(AudioFileLoader):
    def __init__(self, url):
        AudioFileLoader.__init__(self)
        self.url = url

    def getFile(self):
        response = requests.get(self.url, stream=True)
        data, _ = sf.read(io.BytesIO(response.raw.read()))
        self.supply(data);


class DiskLoader(AudioFileLoader):
    maxLeadInSecs = 5

    def __init__(self, inDir):
        AudioFileLoader.__init__(self)
        self.inDir = inDir
        parent_dir = os.path.dirname(inDir)
        self.done_dir = os.path.join(parent_dir, datetime.now().strftime("%Y%m%d_%H%M%S"))
        logger.info(f"Playing files in {inDir}")
        logger.info(f"Moving played files to {self.done_dir}")
        Path(self.done_dir).mkdir()

    def getFile(self):
        rawFiles = os.listdir(self.inDir)
        if len(rawFiles) == 0:
            self.load_state = Loading.NoFiles
            logger.info(f"{self.currently_loading_for.me} has no files")
            return
        logger.info(f"{self.currently_loading_for.me} choosing from {len(rawFiles)} files")
        random.shuffle(rawFiles)
        selected = rawFiles[0]
        leadIn = DiskLoader.maxLeadInSecs * random.random()
        file_to_open = os.path.join(self.inDir, selected)
        data, samplerate = sf.read(file_to_open)
        os.rename(file_to_open, os.path.join(self.done_dir, selected))
        self.supply(data, [0.0] * int(leadIn * samplerate));
    

loader = None
if inDir[:4] == "http":
    loader = HttpFileFetcher(inDir)
else:
    loader = DiskLoader(inDir)

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
        logger.info(f"{self.me} reached the end")
        self.is_ready = False
        self.pos = 0
        empty = [0.0] * size
        empty[:available] = ret
        return empty


class Player():
    blocksize = 16384

    def __init__(self):
        self.should_stop = threading.Event()
        self.loop_player = None
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
        logger.info(f"Process {os.getpid()} ready to play {self.playing} sources")

    def status(self):
        if self.loop_player is None:
            return "stopped"

        if all([s.isFinished() for s in self.sources]):
            return "paused"

        return "playing"

    def play(self):
        if self.loop_player is not None:
            return

        self.should_stop.clear()
        for s in self.sources:
            s.signal_to_start()

        def callback(outdata, frames, time, status):
            if status:
                logger.info(f"callback issue: {status}")

            block = np.dstack([s.read(frames) for s in self.sources]).flatten()
            outdata[:] = struct.pack(f"{self.playing * frames}f", *block)

        def loop():
            logger.info("Playing loop running")
            with sd.RawOutputStream(samplerate=44100, blocksize=Player.blocksize, device=device, channels=self.playing, dtype="float32", callback=callback):
                done = False
                while not done:
                    if self.should_stop.is_set():
                        if all([s.isFinished() for s in self.sources]):
                            done = True
                    time.sleep(1)
                logger.info("Playing loop terminated")
                return

        self.loop_player = threading.Thread(target=loop, args=(), daemon=False)
        self.loop_player.start()

    def stop(self):
        logger.info("received signal to stop")
        if self.loop_player is None:
            logger.info("but not started")
            return

        self.should_stop.set()
        for s in self.sources:
            s.signal_to_stop()

        while self.loop_player.is_alive():
            time.sleep(0.1)
        self.loop_player.join()
        self.loop_player = None
        logger.info("finished")

    def pause(self):
        logger.info("received signal to pause")
        for s in self.sources:
            s.signal_to_stop()

    def resume(self):
        logger.info("received signal to resume")
        for s in self.sources:
            s.signal_to_start()


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
        self.end_headers()

    def do_POST(self):
        getattr(self, "_%s" % self.path[1:])()
        self._standardResponse()
        self.send_header("Content-Length", "0")
        self.end_headers()

    def do_GET(self):
        self._standardResponse()
        status = {"status": player.status()}
        resp = json.dumps(status).encode("utf-8")
        self.send_header("Content-Length", str(len(resp)))
        self.end_headers()
        self.wfile.write(resp)

    def _play(self):
        player.play()

    def _stop(self):
        player.stop()

    def _pause(self):
        player.pause()

    def _resume(self):
        player.resume()


HTTPServer(("0.0.0.0", 9966), Controller).serve_forever()
