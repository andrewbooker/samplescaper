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
leftRelativeToRight = 1.3

class Volume():
    def __init__(self):
        self.volume = 0

    def setTo(self, v):
        vl = v if leftRelativeToRight > 1 else int(v * leftRelativeToRight)
        vr = v if leftRelativeToRight < 1 else int(v / leftRelativeToRight)
        os.system("amixer sset 'Digital' %d%%,%d%%" % (vl, vr))
        self.volume = v

volume = Volume()

class Controller(BaseHTTPRequestHandler):
    def _shutdown(self):
        player.pause()
        os.system("sudo shutdown now")

    def _pause(self):
        player.pause()

    def _resume(self):
        player.resume()

    def _volMin(self):
        volume.setTo(40)

    def _volDown(self):
        volume.setTo(volume.volume - 5)

    def _volUp(self):
        volume.setTo(volume.volume + 5)

    def _volMax(self):
        volume.setTo(95)

    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "null")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _writeVol(self):
        self.wfile.write(json.dumps({"volume": volume.volume}).encode("utf-8"))

    def _sendVol(self):
        self._standardResponse()
        self.end_headers()
        self._writeVol()

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
        self._writeVol()


def startServer():
    HTTPServer(("0.0.0.0", 9966), Controller).serve_forever()

import time
import threading

server = threading.Thread(target=startServer, args=(), daemon=False)
server.start()

startDelayMins = int(config("startDelayMins"))
print("Server started. Playing starts in %d min(s)" % startDelayMins)
time.sleep(startDelayMins * 60)

if volume.volume == 0:
    volume.setTo(int(config("volume")))

player.start()
playingTimeMins = int(config("playingTimeMins"))
print("Player started. Playing stops in %d min(s)" % playingTimeMins)
time.sleep(playingTimeMins * 60)

print("stopping")
del player
shutdownDelayMins = int(config("shutdownDelayMins"))
if shutdownDelayMins > 0:
    time.sleep(shutdownDelayMins * 60)
    os.system("sudo shutdown now")
