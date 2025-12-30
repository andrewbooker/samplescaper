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

ts = datetime.datetime.now().strftime("%Y-%m-%d_%H%M%S")
log_fn = os.path.join("/var/log/randomatones", f"randomtone_playRemote_{ts}.log")
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

def playOneFrom(url, panCh, startedAt):
    log.info(f"fetching from {url}")
    channel = pg.mixer.find_channel()
    if channel is None:
        log.info("%.6f: all channels busy" % time.time())
        return
    try:
        response = requests.get(url, stream=True)
        rawBytes = response.raw.read()
        del response
        bl = len(rawBytes)
        fl = int(bl / 4)
        buf = struct.unpack(f"<{fl}f", rawBytes)
        log.info(f"{len(buf)} samples unpacked")
        sa = np.array([asSample(b, 2, panCh) for b in buf], dtype=np.int16)
        sound = pg.sndarray.make_sound(sa)
    except requests.exceptions.RequestException as e:
        log.info(f"No audio available from {url}")
        time.sleep(10)
        return True

    channel.set_volume(1.0)
    channel.play(sound)
    while channel.get_busy():
        time.sleep(0.1)


def playUntil(url, shouldStop):
    log.info("starting")
    startedAt = time.monotonic()
    threads = []
    start = time.time()
    played = 0
    while not shouldStop.is_set():
        nextSound = threading.Thread(target=playOneFrom, args=(url, played % 2, startedAt), daemon=True)
        nextSound.start()
        threads.append(nextSound)

        for t in threads:
            if not t.is_alive():
                t.join()
                threads.remove(t)

        time.sleep(random.random() * 10)
        played += 1

    for t in threads:
        t.join()
    log.info("stopped")


class Player():
    def __init__(self, numberOfChannels):
        pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
        pg.init()

        random.seed()
        pg.mixer.set_num_channels(numberOfChannels)

        self.serverIp = "192.168.1.88"
        self.shouldStop = threading.Event();
        self.thread = None

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
        self.thread = threading.Thread(target=playUntil, args=(url, self.shouldStop), daemon=True)
        self.thread.start()

player = Player(4)
player.shouldStop.clear()
player.start()
player.thread.join()
