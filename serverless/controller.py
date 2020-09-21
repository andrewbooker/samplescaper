#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json
import re

class SystemVolumeControl(BaseHTTPRequestHandler):
    def _sendVol(self, vol):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()

        d = {"volume": vol}
        self.wfile.write(json.dumps(d).encode("utf-8"))

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
        os.system("shutdown now")
        self.send_response(200)
        self.end_headers()


server = HTTPServer(("0.0.0.0", 9966), SystemVolumeControl)
server.serve_forever()
