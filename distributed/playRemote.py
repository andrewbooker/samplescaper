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


def playOneFrom(url, startedAt):
    print(f"fetching from {url}")
    channel = pg.mixer.find_channel()
    if channel is None:
        print("%.6f: all channels busy" % time.time())
        return
    try:
        response = requests.get(url, stream=True)
        rawBytes = response.raw.read()
        del response
        bl = len(rawBytes)
        fl = int(bl / 4)
        print("expecting", fl, "samples")
        buf = struct.unpack(f"<{fl}f", rawBytes)
        print(len(buf), "floats unpacked")
        print(buf[:2])
        sa = np.array([int(b * (2 ** 15)) for b in buf], dtype=np.int16)
        sound = pg.sndarray.make_sound(sa)
    except requests.exceptions.RequestException as e:
        print("No audio available from %s" % server)
        time.sleep(10)
        return True

    channel.set_volume(1.0)
    channel.play(sound)
    while channel.get_busy():
        time.sleep(0.1)


def playUntil(url, shouldStop):
    print("starting")
    startedAt = time.monotonic()
    threads = []
    start = time.time()
    while not shouldStop.is_set():
        nextSound = threading.Thread(target=playOneFrom, args=(url, startedAt), daemon=True)
        nextSound.start()
        threads.append(nextSound)

        for t in threads:
            if not t.is_alive():
                t.join()
                threads.remove(t)

        time.sleep(random.random() * 10)

    for t in threads:
        t.join()
    print("stopped")


class Player():
    def __init__(self, numberOfChannels):
        pg.mixer.init(frequency=44100, size=-16, channels=1, buffer=1024)
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

player = Player(3)
player.shouldStop.clear()
player.start()
player.thread.join()
