#!/usr/bin/env python

import pygame as pg
import time
import random
import math
import sys

import threading
from datetime import datetime

from availableSamples import *

minDurSecs = (int(sys.argv[4]) if len(sys.argv) > 4 else 3) * 60
tempo = int(sys.argv[5]) if len(sys.argv) > 5 else 120

pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()
pg.mixer.set_num_channels(2)

print("expected finish time %s" % datetime.fromtimestamp(time.time() + minDurSecs + 0.5).strftime("%H:%M"))

class NotePlayer():
    fade = 3
    def __init__(self, number):
        self.stock = [pg.mixer.Sound(f) for f in files[number]]

    def play(self, durationMs):
        channel = pg.mixer.find_channel()
        if channel is not None:
            pan = random.random()
            note = self.stock[random.randint(0, len(self.stock) - 1)]
            channel.set_volume(1.0 - pan, pan)
            channel.play(note, loops = -1, fade_ms = NotePlayer.fade)
            time.sleep(0.001 * (durationMs - (2 * NotePlayer.fade)))
            channel.fadeout(NotePlayer.fade)

noteThreads = []

allNotes = [NotePlayer(n) for n in scale]

def playNote(n, t):
    allNotes[n].play(t)

def launchNote(n, t):
    nt = threading.Thread(target = playNote, args = (n, t), daemon=True)
    nt.start()
    noteThreads.append(nt)

def play(tempo, subdivision, t):
    msPer16th = 15000 / tempo
    beatLengthMs = msPer16th * 16 / subdivision
    end = time.time() + t
    while time.time() < end:
        launchNote(random.randint(0, len(scale) - 1), beatLengthMs)
        time.sleep(beatLengthMs * 0.001)

play(tempo, 16, minDurSecs)
for nt in noteThreads:
    nt.join()


print("done")
