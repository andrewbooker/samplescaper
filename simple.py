#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys
import threading

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

pg.mixer.set_num_channels(42)


class NotePlayer():
    def __init__(self, number):
        self.stock = notes[number]

    def play(self, minDurationSecs):
        end = time.time() + minDurationSecs
        inFlight = []
        while time.time() < end:
            if len(inFlight) < 6:
                channel = pg.mixer.find_channel()
                if channel is not None:
                    inFlight.append(channel)
                    pan = random.random()
                    note = self.stock[random.randint(0, len(self.stock) - 1)]
                    channel.set_volume(1.0 - pan, pan)
                    channel.play(note, fade_ms = fractionLen(note, 0.4))
                    time.sleep(note.get_length() * 0.45)
                    channel.fadeout(fractionLen(note, 0.5))
            else:
                for c in inFlight:
                    if not c.get_busy():
                        inFlight.remove(c)
            time.sleep(0.3)

noteThreads = []

allNotes = [NotePlayer(n) for n in scale]
print("%d note players available" % len(allNotes))


def playNote(n, t):
	allNotes[n].play(t)


def launchNote(n, t):
    nt = threading.Thread(target = playNote, args = (n, t), daemon=True)
    nt.start()
    noteThreads.append(nt)

def playAll(t):
    end = time.time() + t
    while time.time() < end:
        launchNote(random.randint(0, 6), random.randint(5, 30))
        time.sleep(random.randint(2, 10))

playAll(120)
for nt in noteThreads:
    nt.join()

pg.mixer.fadeout(5000)
time.sleep(5)
print("done")