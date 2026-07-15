#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re
import sys
import datetime

class Log:
    def info(self, what):
        print(what)

log = Log()


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


player = Player()

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
        log.info("ignoring vol min")

    def _volDown(self):
        log.info("ignoring vol down")

    def _volUp(self):
        log.info("ignoring vol up")

    def _volMax(self):
        log.info("ignoring vol max")

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
            "volume": 0,
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
