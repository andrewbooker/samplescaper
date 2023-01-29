#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re
import sys
from playPrepped import Player

def config(item):
    with open(sys.argv[2]) as conf:
        c = json.load(conf)
        return c[item]

player = Player(sys.argv[1], 3)
leftRelativeToRight = float(sys.argv[3]) if len(sys.argv) > 3 else 1.0
audioDevice = sys.argv[4] if len(sys.argv) > 4 else "Digital"
print("using audio device", audioDevice)

class Volume():
    def __init__(self, leftRelativeToRight):
        self.volume = 0
        self.lr = leftRelativeToRight

    def setTo(self, v):
        vl = v if leftRelativeToRight > 1 else int(v * self.lr)
        vr = v if leftRelativeToRight < 1 else int(v / self.lr)
        os.system("amixer sset '%s' %d%%,%d%%" % (audioDevice,  vl, vr))
        self.volume = v

class PlayState():
    def __init__(self):
        self.state = "not ready"

    def get(self):
        return self.state

    def set(self, s):
        self.state = s

volume = Volume(leftRelativeToRight)
playState = PlayState()


class Controller(BaseHTTPRequestHandler):
    def _shutdown(self):
        if player is not None:
            player.pause()
        os.system("sudo shutdown now")

    def _play(self):
        if player is not None:
            player.start()
            playState.set("playing")

    def _pause(self):
        if player is not None:
            player.pause()
            playState.set("paused")

    def _resume(self):
        if player is not None:
            player.resume()
            playState.set("playing")

    def _volMin(self):
        volume.setTo(40)

    def _volDown(self):
        volume.setTo(volume.volume - 5)

    def _volUp(self):
        volume.setTo(volume.volume + 5)

    def _volMax(self):
        volume.setTo(100)

    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _writeState(self):
        self.wfile.write(json.dumps({"volume": volume.volume, "state": playState.get()}).encode("utf-8"))

    def _sendVol(self):
        self._standardResponse()
        self.end_headers()
        self._writeState()

    def do_OPTIONS(self):
        self._standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, POST")
        self.end_headers()

    def do_GET(self):
        self._sendVol()

    def do_POST(self):
        self._standardResponse()
        self.end_headers()
        getattr(self, "_%s" % self.path[1:])()
        self._writeState()


def startServer():
    HTTPServer(("0.0.0.0", 9966), Controller).serve_forever()

import time
import threading

server = threading.Thread(target=startServer, args=(), daemon=False)
server.start()

startDelayMins = int(config("startDelayMins"))
print("Server started. ready in %d min(s)" % startDelayMins)
time.sleep(startDelayMins * 60)

if volume.volume == 0:
    volume.setTo(int(config("volume")))

playState.set("ready")

playingTimeMins = int(config("playingTimeMins"))
if playingTimeMins > 0:
    player.start()
    print("Player started. Playing stops in %d min(s)" % playingTimeMins)
    time.sleep(playingTimeMins * 60)

    print("stopping")
    del player
    player = None
    shutdownDelayMins = int(config("shutdownDelayMins"))
    if shutdownDelayMins > 0:
        time.sleep(shutdownDelayMins * 60)
        os.system("sudo shutdown now")

