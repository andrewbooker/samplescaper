#!/usr/bin/env python

import pygame as pg
import requests
import time

pg.mixer.init(frequency=44100, size=-16, channels=1, buffer=1024)
pg.init()

url = "http://localhost:3064"
response = requests.get(url, stream=True)
note = pg.mixer.Sound(response.raw.read())
del response

fadeInSecs = 1
fadeOutSecs = 10

channel = pg.mixer.find_channel()
channel.set_volume(1.0)
channel.play(note, loops = -1, fade_ms = fadeInSecs * 1000)
time.sleep(fadeInSecs)
channel.fadeout(fadeOutSecs * 1000)
time.sleep(fadeOutSecs)

pg.mixer.quit()
pg.quit()

