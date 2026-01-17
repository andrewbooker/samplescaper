#!/usr/bin/env python

import pygame as pg
import time
import datetime
import os
import sys
import random
import threading
import requests
import struct
import numpy as np
import datetime
import logging
import io

ts = datetime.datetime.now().strftime("%Y-%m-%d_%H%M%S")
log_fn = os.path.join("/var/log/randomatones", f"distributed_client_{ts}.log")
logging.basicConfig(filename=log_fn, level=logging.INFO)

def createLog(module_name):
    log = logging.getLogger(module_name)
    sys.stdout.write = log.info
    sys.stderr.write = log.error
    return log

log = createLog("playRemote")


def asSample(v, chs, ch):
    s = [0.0] * chs
    s[ch] = int(v * (2 ** 15))
    return s

def playOneFrom(url, panCh, startedAt, soundListener):
    log.info(f"fetching from {url}")
    channel = pg.mixer.find_channel()
    if channel is None:
        log.info("%.6f: all channels busy" % time.time())
        return

    sound = None
    isSilence = True
    try:
        buf = io.BytesIO()
        response = requests.get(url, stream=True)
        response.raise_for_status()
        for chunk in response.iter_content(chunk_size=8192):
            if not chunk:
                continue
            buf.write(chunk)

        buf.seek(0)
        rawBytes = buf.getvalue()
        bl = len(rawBytes)
        isSilence = response.status_code != 200
        log.info(f"response {response.status_code} fetching {bl} bytes")
        del response
        fl = int(bl / 4)
        buf = struct.unpack(f"<{fl}f", rawBytes)
        log.info(f"{len(buf)} samples unpacked")
        sa = np.array([asSample(b, 2, panCh) for b in buf], dtype=np.int16)
        sound = pg.sndarray.make_sound(sa)
    except requests.exceptions.RequestException as e:
        log.info(f"No audio available from {url}")
        log.info(e)
        time.sleep(10)
        return

    if sound is None:
        return
    channel.set_volume(1.0)
    channel.play(sound)
    if not isSilence:
        soundListener.startOne()
    while channel.get_busy():
        time.sleep(0.1)
    if not isSilence:
        soundListener.stopOne()
    del sound


def playUntil(shouldStop, url, soundListener):
    log.info("starting")
    startedAt = time.monotonic()
    threads = []
    start = time.time()
    played = 0
    while not shouldStop.is_set():
        nextSound = threading.Thread(target=playOneFrom, args=(url, played % 2, startedAt, soundListener), daemon=True)
        nextSound.start()
        threads.append(nextSound)

        for t in threads:
            if not t.is_alive():
                t.join()
                threads.remove(t)

        played += 1
        time.sleep(random.random() * 10)

    for t in threads:
        t.join()
    log.info("stopped")


class SoundListener:
    def startOne(self):
        pass

    def stopOne(self):
        pass


class Player():
    def __init__(self, numberOfChannels, soundListener, serverIp):
        pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
        pg.init()

        random.seed()
        pg.mixer.set_num_channels(numberOfChannels)

        self.serverIp = serverIp
        self.shouldStop = threading.Event();
        self.thread = None
        self.soundListener = soundListener

    def __del__(self):
        self.pause()
        pg.quit()

    def pause(self):
        self.shouldStop.set()
        if self.thread is not None:
            self.thread.join()

    def start(self):
        self.resume()

    def resume(self):
        if not self.shouldStop.is_set() and self.thread is not None:
            return

        url = f"http://{self.serverIp}:3064"
        self.shouldStop.clear()
        self.thread = threading.Thread(target=playUntil, args=(self.shouldStop, url, self.soundListener), daemon=True)
        self.thread.start()


class MotorRunner(SoundListener):
    def __init__(self):
        self.count = 0
        self.url = "http://0.0.0.0"
        self.direction = "clockwise"

    def _send(self, action):
        try:
            response = requests.post(f"{self.url}:9977/{action}")
            log.info(f"{action} response: {response.status_code}")
        except requests.exceptions.ConnectionError as e:
            print(e)

    def startOne(self):
        if self.count < 1:
            log.info("starting motor")
            self._send(self.direction)
        self.count += 1
        log.info(f"{self.count} playing on start")

    def stopOne(self):
        self.count -= 1
        log.info(f"{self.count} playing on stop")
        if self.count < 1:
            log.info("stopping motor")
            self._send(f"{self.direction}Stop")
            self.direction = "antiClockwise" if self.direction == "clockwise" else "clockwise"


serverIp = sys.argv[1] if len(sys.argv) > 1 else "192.168.1.88"
soundListener = MotorRunner()
player = Player(4, soundListener, serverIp)
player.shouldStop.clear()
player.start()
player.thread.join()
