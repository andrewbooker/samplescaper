#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re
import sys
import datetime
from playPrepped import Player
from volume import SystemVolume

from processLogging import createLog
log = createLog(__name__)


configLoc = sys.argv[2]
def config(item):
    with open(os.path.join(configLoc, "config.json")) as conf:
        c = json.load(conf)
        return c[item]

class Key():
    modes = {
        "aeolian": [2, 1, 2, 2, 1, 2],
        "dorian": [2, 1, 2, 2, 2, 1],
        "ionian": [2, 2, 1, 2, 2, 2],
        "mixolydian": [2, 2, 1, 2, 2, 1],
        "lydian": [2, 2, 2, 1, 2, 2],
        "wholetone": [2, 2, 2, 2, 2],
        "minorpentatonic": [3, 2, 2, 3],
        "majorpentatonic": [4, 1, 2, 4]
    }

    def __init__(self):
        self.fqFn = os.path.join(configLoc, "key.json")
        conf = open(self.fqFn)
        self.key = json.load(conf)
        conf.close()

    def _write(self):
        with open(self.fqFn, "w") as conf:
            conf.write(json.dumps(self.key, indent=4))

    def getTonic(self):
        return self.key["tonic"]

    def getMode(self):
        for m, a in Key.modes.items():
            if a == self.key["mode"]:
                return m
        return [*Key.modes][0]

    def setTonic(self, t):
        self.key["tonic"] = t
        self._write()

    def setMode(self, m):
        self.key["mode"] = Key.modes[m]
        self._write()


maxVol = 95
player = Player(sys.argv[1], 3)
leftRelativeToRight = float(sys.argv[3]) if len(sys.argv) > 3 else 1.0

class Volume():
    def __init__(self, leftRelativeToRight):
        self.systemVolume = SystemVolume()
        self.lr = leftRelativeToRight
        self.volumeCoeff = 0.7 if "Master" in self.systemVolume.deviceName else 1.0
        self._update()
        log.info(f"using audio device {self.systemVolume.deviceName} volume coeff {self.volumeCoeff} current vol {self.volume}")

    def _update(self):
        vols = [v for v in self.systemVolume.get()]
        vols.sort(reverse=True)
        self.volume = vols[0] / self.volumeCoeff

    def setTo(self, v):
        vl = v if leftRelativeToRight > 1 else int(v * self.lr)
        vr = v if leftRelativeToRight < 1 else int(v / self.lr)

        self.systemVolume.set(int(vl * self.volumeCoeff), int(vr * self.volumeCoeff))
        self._update()

class PlayState():
    def __init__(self):
        self.state = "not started"

    def get(self):
        if "not" in self.state:
            numFiles = len(os.listdir(os.path.join(sys.argv[1], "raw")))
            if numFiles < 6:
                return "built %d/%d" % (numFiles, 6)
            else:
                self.state = "ready"
        return self.state

    def set(self, s):
        self.state = s

volume = Volume(leftRelativeToRight)
key = Key()
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
        volume.setTo(max(volume.volume - 5, 0))

    def _volUp(self):
        volume.setTo(min(volume.volume + 5, maxVol))

    def _volMax(self):
        volume.setTo(maxVol)

    def _setTime(self):
        os.system("sudo date --set %s" % self.headers.get("Current-Time"))

    def _setTonic(self):
        key.setTonic(int(self.headers.get("Tonic")))

    def _setMode(self):
        key.setMode(self.headers.get("Mode"))

    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _writeState(self):
        self.wfile.write(json.dumps({
            "volume": int(volume.volume),
            "state": playState.get(),
            "time": datetime.datetime.now().strftime("%d %b %Y %H:%M"),
            "tonic": key.getTonic(),
            "mode": key.getMode()
        }).encode("utf-8"))

    def _sendVol(self):
        self._standardResponse()
        self.end_headers()
        self._writeState()

    def do_OPTIONS(self):
        self._standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, POST")
        self.send_header("Access-Control-Allow-Headers", "Current-Time")
        self.send_header("Access-Control-Allow-Headers", "Tonic")
        self.send_header("Access-Control-Allow-Headers", "Mode")
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

if volume.volume == 0:
    volume.setTo(int(config("volume")))

playingTimeMins = int(config("playingTimeMins"))
if playingTimeMins > 0:
    player.start()
    log.info(f"Player started. Playing stops in {playingTimeMins} min(s)")
    time.sleep(playingTimeMins * 60)

    log.info("stopping")
    del player
    player = None
    shutdownDelayMins = int(config("shutdownDelayMins"))
    if shutdownDelayMins > 0:
        time.sleep(shutdownDelayMins * 60)
        os.system("sudo shutdown now")

