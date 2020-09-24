#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re

class SystemVolumeControl(BaseHTTPRequestHandler):
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
        print(self.headers.get("Origin"))
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
        os.system("sudo shutdown now")


server = HTTPServer(("0.0.0.0", 9966), SystemVolumeControl)
server.serve_forever()
