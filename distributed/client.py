#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import requests
import time
import io
import random
import json
import os

serversFile = "./servers_client.json"


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
    if os.path.exists(serversFile) and os.stat(serversFile).st_size > 2:
        with open(serversFile, "r") as sl:
            servers += json.load(sl)

    if len(servers) == 0:
        print("Stopping. No servers available.")
        return False

    server = anyItemIn(servers)
    url = "http://%s:3064" % server

    pan = random.random()
    totalTime = 5.0 + (10.0 * random.random())
    
    sound = []
    minLength = 0
    sampleRate = 44100
    
    try:
        response = requests.get(url, stream=True)
        data, sr = sf.read(io.BytesIO(response.raw.read()))
        del response
        sampleRate = sr
        minLength = int(totalTime * sampleRate)
        while len(sound) < minLength:
            sound += [d for d in data]
    except requests.exceptions.RequestException as e:
        print("No audio available from %s" % server)
        time.sleep(10)
        return True
    
    vol = Volume(len(sound), 0.1)

    sd.play([applyPan(vol.vol() * s, pan) for s in sound], sampleRate)
    time.sleep(len(sound) / (1.0 * sampleRate))
    return True


def discover():
    found = []
    base = "192.168.0"
    print("looking for servers...")
    for r in range(64):
        server = "%s.%d" % (base, r)
        url = "http://%s:%d" % (server, 3066)
        try:
            response = requests.head(url, timeout=0.1)
            if (response.status_code == 200):
                found.append(server)
                print("found", server)
            del response
        except requests.exceptions.RequestException as e:
            pass

    with open(serversFile, "w") as sl:
        json.dump(found, sl)

discover()
canContinue = True
while canContinue:
    canContinue = playOne()




