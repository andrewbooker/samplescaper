#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
import json

class SystemVolumeControl(BaseHTTPRequestHandler):
    def do_PUT(self):
        l = int(self.headers.get("content-length", 0))
        data = self.rfile.read(l)
        obj = json.loads(data.decode())
        vol = int(obj["volume"])
        os.system("amixer sset 'Digital' %d%%" % vol)
        self.send_response(200)
        self.send_header("Content-Type", "application/json")   
        self.end_headers()
        
        d = {"volume": vol}
        self.wfile.write(json.dumps(d).encode("utf-8"))


server = HTTPServer(("0.0.0.0", 9966), SystemVolumeControl)
server.serve_forever()
