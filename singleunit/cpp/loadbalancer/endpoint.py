#!/usr/bin/env python

import random
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler


RESPONSE = random.random()
PORT = int(sys.argv[1])

class SampleServer(BaseHTTPRequestHandler):
    def do_GET(self):
        size = 1
        resp = f"{PORT} {RESPONSE}\n"
        sys.stdout.write(f"sending {RESPONSE:.4f} on {PORT}\n\r")
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", len(resp))
        self.end_headers()
        self.wfile.write(resp.encode())
        return None


HTTPServer(("0.0.0.0", PORT), SampleServer).serve_forever()
