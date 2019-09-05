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

scale = [48, 50, 51, 53, 55, 56, 58]

path = sys.argv[1]

files = {}
for s in scale:
    notePath = join(path, str(s))
    files[s] = [join(notePath, f) for f in listdir(notePath) if isfile(join(notePath, f))]

notes = {n: [pg.mixer.Sound(f) for f in files[n]] for n in files.keys()}

def fractionLen(note, f):
    return math.floor(note.get_length() * 1000.0 * f)

pg.mixer.set_num_channels(30)

def playNote(n, t):
	end = time.time() + t
	while time.time() < end:
		channel = pg.mixer.find_channel()
		if channel is not None:
			pan = random.random()
			note = notes[n][random.randint(0, len(notes[n]) - 1)]
			channel.set_volume(1.0 - pan, pan)
			channel.play(note, fade_ms = fractionLen(note, 0.4))
			time.sleep(note.get_length() * 0.45)
			channel.fadeout(fractionLen(note, 0.5))
		else:
			time.sleep(0.3)

for s in scale:
    playNote(s, 6)

pg.mixer.fadeout(5000)
time.sleep(5)
print("done")