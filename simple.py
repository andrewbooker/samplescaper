#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys

pg.mixer.init()
pg.init()

from os import listdir
from os.path import isfile, join

path = sys.argv[1]
files = [join(path, f) for f in listdir(path) if isfile(join(path, f))]

notes = [pg.mixer.Sound(f) for f in files]

def frLen(sound, f):
    return math.floor(sound.get_length() * 1000.0 * f)

pg.mixer.set_num_channels(50)
channels = []

for i in range(10):
    n = i if i < len(notes) else i % len(notes)

    channel = pg.mixer.find_channel()
    pan = random.random()
    note = notes[n]
    channel.set_volume(1.0 - pan, pan)
    channel.play(note, fade_ms=random.randint(frLen(note, 0.1), frLen(note, 0.3)))
    channel.fadeout(frLen(note, 0.5))
    channels.append(channel)

    time.sleep(0.3 + random.random())

print("created %d channels" % len(channels))
pg.mixer.fadeout(5000)
time.sleep(5)
print("done")