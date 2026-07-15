#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re
import sys
import datetime
from volume import SystemVolume, log


class Player():
    def __init__(self):
        self.state = "playing"

    def __del__(self):
        self.stop()

    def _instruct(self, k):
        os.system(f"tmux send-keys -t 1 {k}")

    def getState(self):
        return self.state

    def stop(self):
        self._instruct("q")

    def pause(self):
        if self.state == "playing":
            self._instruct("p")
            self.state = "paused"

    def start(self):
        self.resume()

    def resume(self):
        if self.state == "paused":
            self._instruct("r")
            self.state = "playing"


maxVol = 95

class Volume():
    def __init__(self, leftRelativeToRight):
        self.systemVolume = SystemVolume()
        self.lr = leftRelativeToRight
        volume_dev = self.systemVolume.device()
        self.volumeCoeff = 0.7 if volume_dev is not None and "Master" in volume_dev else 1.0
        self._update()
        if volume_dev is not None:
            log.info(f"using audio device {volume_dev} volume coeff {self.volumeCoeff} current vol {self.volume}")
        else:
            log.info("Volume device not known at startup")

    def _update(self):
        vols = [v for v in self.systemVolume.get()]
        vols.sort(reverse=True)
        self.volume = vols[0] / self.volumeCoeff

    def setTo(self, v):
        vl = v if self.lr > 1 else int(v * self.lr)
        vr = v if self.lr < 1 else int(v / self.lr)

        self.systemVolume.set(int(vl * self.volumeCoeff), int(vr * self.volumeCoeff))
        self._update()



player = Player()
leftRelativeToRight = float(sys.argv[3]) if len(sys.argv) > 3 else 1.0
volume = Volume(leftRelativeToRight)


class Controller(BaseHTTPRequestHandler):
    def _shutdown(self):
        if player is not None:
            player.stop()
        os.system("sudo shutdown now")

    def _play(self):
        if player is not None:
            player.start()

    def _pause(self):
        if player is not None:
            player.pause()

    def _resume(self):
        if player is not None:
            player.resume()

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
        log.info("ignoring tonic")

    def _setMode(self):
        log.info("ignoring mode")

    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _writeState(self):
        self.wfile.write(json.dumps({
            "volume": int(volume.volume),
            "state": player.getState(),
            "time": datetime.datetime.now().strftime("%d %b %Y %H:%M"),
            "tonic": 57,
            "mode": "aeolian"
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
