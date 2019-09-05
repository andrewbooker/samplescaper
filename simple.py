#!/usr/bin/env python

import pygame as pg
import time
import random
import math

pg.mixer.init()
pg.init()

a1Note = pg.mixer.Sound("d:\\Samples\\Voice\\Long\\48\\shortSample003.wav")
a2Note = pg.mixer.Sound("d:\\Samples\\Voice\\Long\\48\\shortSample007.wav")


def frLen(sound, f):
    return math.floor(a1Note.get_length() * 1000.0 * f)


pg.mixer.set_num_channels(50)
channels = []

for i in range(10):
    channel1 = pg.mixer.find_channel()
    pan1 = random.random()
    channel1.set_volume(1.0 - pan1, pan1)
    channel1.play(a1Note, fade_ms=random.randint(frLen(a1Note, 0.1), frLen(a1Note, 0.3)))
    channel1.fadeout(frLen(a1Note, 0.5))
    channels.append(channel1)

    time.sleep(0.3 + random.random())
    
    channel2 = pg.mixer.find_channel()
    pan2 = random.random()
    channel2.set_volume(1.0 - pan2, pan2)
    channel2.play(a2Note, fade_ms=random.randint(frLen(a2Note, 0.1), frLen(a2Note, 0.3)))
    channel2.fadeout(frLen(a1Note, 0.5))
    channels.append(channel1)
 
    time.sleep(0.3 + random.random())
print("created %d channels" % len(channels))
pg.mixer.fadeout(5000)
time.sleep(5)
print("done")