#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re
import sys
from playPrepped import Player

players = int(sys.argv[2]) if len(sys.argv) > 2 else 3
startDelayMins = int(sys.argv[3]) if len(sys.argv) > 3 else 5
playingTimeDelayMins = int(sys.argv[4]) if len(sys.argv) > 4 else 5
player = Player(sys.argv[1], players)


class Controller(BaseHTTPRequestHandler):
    def _archive(self):
        os.system("zip -r ~/Music/archives/$(date +\"%Y%m%d_%H%M%S\").zip ~/Music/20*")
        os.system("rm -rf ~/Music/20*")

    def _shutdown(self):
        player.pause()
        self._archive()
        os.system("sudo shutdown now")

    def _updateAndRestart(self):
        player.pause()
        os.system("cd ..; git pull --rebase")
        os.system("sudo shutdown -r now")

    def _pause(self):
        player.pause()

    def _resume(self):
        player.resume()

    def _standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "null")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _sendVol(self, vol):
        self._standardResponse()
        self.end_headers()

        d = {"volume": vol}
        self.wfile.write(json.dumps(d).encode("utf-8"))

    def do_OPTIONS(self):
        self._standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, PUT, POST")
        self.end_headers()

    def do_GET(self):
        sv = os.popen("amixer sget 'Digital'").read().split("\n")[-2]
        self._sendVol(int(re.search("\[(\d+)%\]", sv).group(1)))

    def do_PUT(self):
        l = int(self.headers.get("content-length", 0))
        data = self.rfile.read(l)
        obj = json.loads(data.decode())
        vol = int(obj["volume"])
        os.system("amixer sset 'Digital' %d%%" % vol)
        self._sendVol(vol)

    def do_POST(self):
        self._standardResponse()
        self.end_headers()
        self.wfile.write(json.dumps({}).encode("utf-8"))
        fn = getattr(self, "_%s" % self.path[1:])()


def startServer():
    HTTPServer(("0.0.0.0", 9966), Controller).serve_forever()

import time
import threading

server = threading.Thread(target=startServer, args=(), daemon=False)
server.start()

print("Server started. Playing starts in %d min(s)" % startDelayMins)
time.sleep(startDelayMins * 60)

os.system("amixer sset 'Digital' %d%%" % 75)
player.start()
print("Player started. Playing stops in %d min(s)" % playingTimeDelayMins)
time.sleep(playingTimeDelayMins * 60)

print("stopping")
del player
os.system("sudo shutdown now")
