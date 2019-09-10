#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys
import threading
from datetime import datetime

pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()

from os import listdir
from os.path import isfile, join

fadeInSecs = 1
fadeOutSecs = 10

path = sys.argv[1]
root = sys.argv[2] if len(sys.argv) > 2 else "E"
mode = sys.argv[3] if len(sys.argv) > 3 else "aeolian"
minDurSecs = (int(sys.argv[4]) if len(sys.argv) > 4 else 3) * 60
noteRange = [int(n) for n in listdir(path)]
print("range available")
print(noteRange)

import scale as modal

modalNotes = modal.Scale(7, modal.roots[root], modal.modes[mode]).notes

for i in range(7):
	if modalNotes[i] + 12 in noteRange:
		modalNotes.append(modalNotes[i] + 12)
	if modalNotes[i] - 12 in noteRange:
		modalNotes.append(modalNotes[i] - 12)
		
scale = [n for n in modalNotes if n in noteRange]
print("%d notes available in %s %s" % (len(scale), root, mode))
print(scale)
print("expected finish time %s" % datetime.fromtimestamp(time.time() + minDurSecs + 0.5).strftime("%H:%M"))


files = {}
for s in scale:
    notePath = join(path, str(s))
    files[s] = [join(notePath, f) for f in listdir(notePath) if isfile(join(notePath, f))]

maxInflightPerNote = 6
pg.mixer.set_num_channels(maxInflightPerNote * len(scale))


class NotePlayer():
    def __init__(self, number):
        self.stock = [pg.mixer.Sound(f) for f in files[number]]

    def play(self, minDurationSecs):
        end = time.time() + minDurationSecs
        inFlight = []
        while time.time() < end:
            if len(inFlight) < maxInflightPerNote:
                channel = pg.mixer.find_channel()
                if channel is not None:
                    inFlight.append(channel)
                    pan = random.random()
                    note = self.stock[random.randint(0, len(self.stock) - 1)]
                    channel.set_volume(1.0 - pan, pan)
                    channel.play(note, loops = -1, fade_ms = fadeInSecs * 1000)
                    time.sleep(fadeInSecs)
                    channel.fadeout(fadeOutSecs * 1000)
            else:
                for c in inFlight:
                    if not c.get_busy():
                        inFlight.remove(c)
            time.sleep(0.3)

noteThreads = []

allNotes = [NotePlayer(n) for n in scale]

def playNote(n, t):
    allNotes[n].play(t)


def launchNote(n, t):
    nt = threading.Thread(target = playNote, args = (n, t), daemon=True)
    nt.start()
    noteThreads.append(nt)

def playAll(t):
    end = time.time() + t
    while time.time() < end:
        launchNote(random.randint(0, len(scale) - 1), random.randint(5, 30))
        time.sleep(random.randint(2, 10))

playAll(minDurSecs)
for nt in noteThreads:
    nt.join()

pg.mixer.fadeout(fadeOutSecs * 1000)
time.sleep(fadeOutSecs + 1)
print("done")
