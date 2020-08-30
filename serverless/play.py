#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys
import os

def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])

def playOneFrom(poolDir):
    channel = pg.mixer.find_channel()
    if channel is None:
        return

    f = nextAudioFileFrom(poolDir)
    print(f)
    sys.stdout.write("\r")
    sound = pg.mixer.Sound(f)
    fadeInSecs = random.random() + 0.5
    fadeOutSecs = (10.0 * random.random()) + 5.0
    totalTime = fadeInSecs + fadeOutSecs + 1.0
    start = time.time()

    channel.set_volume(1.0, 0.0)
    channel.play(sound, loops = -1, fade_ms = int(fadeInSecs * 1000))
    fadingOut = False
    dt = 0
    incr = 0.01
    while channel.get_busy():
        time.sleep(incr)
        dt += incr
        if not fadingOut and dt > fadeInSecs:
            channel.fadeout(int(fadeOutSecs * 1000))

        p = dt / totalTime
        if p >= 1.0:
            channel.stop()
        else:
            channel.set_volume(1.0 - p, p)


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

poolDir = sys.argv[1]

pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()

random.seed()
maxInflightPerNote = 6
pg.mixer.set_num_channels(maxInflightPerNote)


import readchar
import threading


shouldStop = threading.Event()


playAll = threading.Thread(target=playContinuouslyFrom, args=(poolDir,shouldStop), daemon=True)
playAll.start()

print("Started. Press 'q' to exit")
while not shouldStop.is_set():
    c = readchar.readchar()
    if c == "q":
        print("Stopping...")
        shouldStop.set()

playAll.join()
