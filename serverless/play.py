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
    sound = pg.mixer.Sound(f)
    fadeInSecs = random.random() + 0.5
    fadeOutSecs = (10.0 * random.random()) + 5.0

    channel.set_volume(1.0, 1.0)
    channel.play(sound, loops = -1, fade_ms = int(fadeInSecs * 1000))
    time.sleep(fadeInSecs)
    channel.fadeout(int(fadeOutSecs * 1000))
    time.sleep(fadeOutSecs + 1)


poolDir = sys.argv[1]

pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()

random.seed()
maxInflightPerNote = 6
pg.mixer.set_num_channels(maxInflightPerNote)


playOneFrom(poolDir)
