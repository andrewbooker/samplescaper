#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys
import os

class Sine():
    def __init__(self, periodSecs, incrSecs):
        self.radsPerSec = incrSecs * 2 * math.pi / periodSecs

    def _nextQuadrantAfter(self, theta, q):
        if q == 0 and theta > math.pi / 2.0:
            return 1
        if q == 1 and theta < 0:
            return 2
        if q == 2 and theta < math.pi / -2.0:
            return 3
        if q == 3 and theta > 0:
            return 0
        return q

    def nextValAfter(self, last):
        theta = math.asin((2 * last["value"]) - 1.0)
        incr = -1.0 if last["quadrant"] in [1, 2] else 1.0
        theta += (incr * self.radsPerSec)

        return {"value": 0.5 * (1.0 + math.sin(theta)), "quadrant": self._nextQuadrantAfter(theta, last["quadrant"])}

    @staticmethod
    def randomStart():
        r = (random.random(), int(2 * random.random()))
        val = r[0]
        q = r[1] if val > 0.5 else 2 + r[1]
        return {"value": val, "quadrant": q}


def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])

def playOneFrom(poolDir):
    channel = pg.mixer.find_channel()
    if channel is None:
        sys.stdout.write("%.6f: all channels busy\n\r" % time.time())
        return

    f = nextAudioFileFrom(poolDir)
    if f is None:
        sys.stdout.write("%.6f: No files in %s\n\r" % (time.time(), poolDir))
        return

    sound = pg.mixer.Sound(f)
    fadeInSecs = (3 * random.random()) + 0.5
    fadeOutSecs = (10 * random.random()) + 3.0
    totalTime = fadeInSecs + fadeOutSecs + 1.0
    start = time.time()
    sys.stdout.write("%.6f: %s\n\r" % (start, f))

    incr = 0.01
    pan = Sine(2.0 + (5 * random.random()), incr)
    p = Sine.randomStart()
    channel.set_volume(1.0 - p["value"], p["value"])
    channel.play(sound, loops = -1, fade_ms = int(fadeInSecs * 1000))
    fadingOut = False
    dt = 0

    while channel.get_busy():
        time.sleep(incr)
        dt += incr
        if not fadingOut and dt > fadeInSecs:
            channel.fadeout(int(fadeOutSecs * 1000))

        if (dt / totalTime) >= 1.0:
            channel.stop()
        else:
            p = pan.nextValAfter(p)
            channel.set_volume(1.0 - p["value"], p["value"])

import threading
def playContinuouslyFrom(poolDir, shouldStop):
    threads = []
    while not shouldStop.is_set():
        nextSound = threading.Thread(target=playOneFrom, args=(poolDir,), daemon=True)
        nextSound.start()
        threads.append(nextSound)

        for t in threads:
            if not t.is_alive():
                t.join()
                threads.remove(t)

        time.sleep(random.random() * 10)

    for t in threads:
        t.join()

class NeverStop():
    def is_set(self):
        return False

poolDir = sys.argv[1]
pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()

random.seed()
pg.mixer.set_num_channels(3)

shouldStop = NeverStop()
playContinuouslyFrom(poolDir, shouldStop)
