#!/usr/bin/env python

import pygame as pg
import requests
import random
import time




import shutil
import requests

audioFile = "./thing.wav"

url = "http://localhost:3064"
response = requests.get(url, stream=True)
with open(audioFile, "wb") as dump:
    shutil.copyfileobj(response.raw, dump)
del response


fadeInSecs = 1
fadeOutSecs = 10


pg.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
pg.init()

note = pg.mixer.Sound(audioFile)
channel = pg.mixer.find_channel()
pan = random.random()
channel.set_volume(1.0 - pan, pan)
channel.play(note, loops = -1, fade_ms = fadeInSecs * 1000)
time.sleep(fadeInSecs)
channel.fadeout(fadeOutSecs * 1000)
time.sleep(fadeOutSecs)

