#!/usr/bin/env python

import pygame as pg
import time
import datetime
import os
from pathlib import Path
import sys
import random
import threading


def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])

def playOneFrom(poolDir, startedAt, playedDir):
    channel = pg.mixer.find_channel()
    if channel is None:
        sys.stdout.write("%.6f: all channels busy\n\r" % time.time())
        return

    f = nextAudioFileFrom(poolDir)
    if f is None:
        sys.stdout.write("%.6f: No files in %s\n\r" % (time.time(), poolDir))
        return

    sound = pg.mixer.Sound(f)
    with open(os.path.join(playedDir, "inventory.lof"), "a") as lof:
        lof.write("file \"%s\" offset %f\n" % (os.path.join(sys.argv[1], "looped", os.path.basename(f)), time.monotonic() - startedAt))

    channel.set_volume(1.0)
    channel.play(sound)
    while channel.get_busy():
        time.sleep(0.1)
        
    if not Path(os.path.join(playedDir, os.path.basename(f))).exists():
        if len(os.listdir(poolDir)) > 7:
            sys.stdout.write("%.6f: moving to %s to %s\n\r" % (time.monotonic(), f, playedDir))
            os.system("mv %s %s" % (f, playedDir))
        else:
            sys.stdout.write("%.6f: copying %s to %s\n\r" % (time.monotonic(), f, playedDir))
            os.system("cp %s %s" % (f, playedDir))
    else:
        sys.stdout.write("%.6f: already stored %s\n\r" % (time.monotonic(), f))


def playUntil(inDir, shouldStop):
    poolDir = os.path.join(inDir, "looped")
    print("Playing from", poolDir)
    playedDir = os.path.join(Path(sys.argv[1]).parent, datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S"))
    os.mkdir(playedDir)
    startedAt = time.monotonic()
    threads = []
    start = time.time()
    while not shouldStop.is_set():
        nextSound = threading.Thread(target=playOneFrom, args=(poolDir, startedAt, playedDir), daemon=True)
        nextSound.start()
        threads.append(nextSound)

        for t in threads:
            if not t.is_alive():
                t.join()
                threads.remove(t)

        time.sleep(random.random() * 10)

    for t in threads:
        t.join()


class Player():
    def __init__(self, inDir, numberOfChannels):
        pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
        pg.init()

        random.seed()
        pg.mixer.set_num_channels(numberOfChannels)

        self.inDir = inDir
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

        self.shouldStop.clear()
        self.thread = threading.Thread(target=playUntil, args=(self.inDir, self.shouldStop), daemon=True)
        self.thread.start()


