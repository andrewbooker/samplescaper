#!/usr/bin/env python

import pygame as pg
import time
import os
import sys
import random


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
    sys.stdout.write("%.6f: %s\n\r" % (time.time(), f))

    channel.set_volume(1.0)
    channel.play(sound)
    while channel.get_busy():
        time.sleep(0.1)


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
pg.mixer.set_num_channels(3)

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



