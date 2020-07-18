#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import requests
import time
import io
import random
import json


class Volume():

    def __init__(self, totalLength, upTimePercent):
        self.pos = 0
        self.totalLength = totalLength
        self.upLength = upTimePercent * totalLength

    def vol(self):
        self.pos += 1
        if self.pos > self.totalLength:
            return 0
        if self.pos > self.upLength:
            return 1.0 - ((self.pos - self.upLength) / (self.totalLength - self.upLength))
        return self.pos / self.upLength

def applyPan(s, pan):
    return [pan * s, (1.0 - pan) * s]

def anyItemIn(arr):
    if len(arr) == 1:
        return arr[0]

    return arr[random.randint(0, len(arr) - 1)]

sd.default.channels = 2


def playOne():
    servers = []
    with open("./servers.json", "r") as sl:
        servers += json.load(sl)

    if len(servers) == 0:
        print("stopping. no servers available.")
        return False

    server = anyItemIn(servers)
    url = "http://%s:3064" % server
    response = requests.get(url, stream=True)
    data, sampleRate = sf.read(io.BytesIO(response.raw.read()))
    del response

    pan = random.random()
    totalTime = 5.0 + (10.0 * random.random())
    minLength = int(totalTime * sampleRate)
    sound = []
    while len(sound) < minLength:
        sound += [d for d in data]

    vol = Volume(len(sound), 0.1)

    sd.play([applyPan(vol.vol() * s, pan) for s in sound], sampleRate)
    time.sleep(len(sound) / (1.0 * sampleRate))
    return True


canContinue = True
while canContinue:
    canContinue = playOne()




