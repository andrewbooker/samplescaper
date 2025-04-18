#!/usr/bin/env python

import math
import random
import struct
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler, SimpleHTTPRequestHandler


SAMPLE_RATE = 44100

def freq(n):
    return math.pow(2, (n - 69) / 12.0) * 440

#make 44100 16 or 32 bit array of samples

class SampleServer(BaseHTTPRequestHandler):
    def do_GET(self):
        size = int(SAMPLE_RATE * 1.0)
        note = random.randint(50, 80)
        buff = [math.sin(2 * math.pi * freq(note) * i / SAMPLE_RATE) for i in range(size)];
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", size * 4)
        self.end_headers()
        self.wfile.write(struct.pack(f"{size}f", *buff))
        sys.stdout.write(f"generated {size} samples\n\r")
        return None


HTTPServer(("0.0.0.0", 9965), SampleServer).serve_forever()
