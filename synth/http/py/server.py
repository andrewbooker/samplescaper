#!/usr/bin/env python

import math
import random
import struct
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

SAMPLE_RATE = 44100

def freq(n):
    return math.pow(2, (n - 69) / 12.0) * 440

def anywhere_between(v1, v2):
    return v1 + (random.random() * (v2 - v1))


class Envelope:
    def at(self, i):
        pass


class Constant(Envelope):
    def __init__(self, v):
        self.v = v

    def at(self, i):
        return self.v


class Amplitude(Envelope):
    def __init__(self, over):
        self.over = over
        self.ramp_up = int(SAMPLE_RATE * anywhere_between(2.0, 4.0))
        self.ramp_down = int(anywhere_between(0.2 * over, 0.5 * over))
        self.start_ramp_down = self.over - self.ramp_down

    def at(self, i):
        if i < self.ramp_up:
            return i * 1.0 / self.ramp_up
        if i > self.start_ramp_down:
            return 1.0 - ((i - self.start_ramp_down) * 1.0 / self.ramp_down)
        return 1.0


class AmplitudeModulator(Envelope):
    def __init__(self, depth_envelope):
        self.freq = anywhere_between(0.3, 7.0)
        self.depth = depth_envelope

    def at(self, i):
        d = self.depth.at(i)
        v = 0.5 * (1.0 + math.cos(2 * math.pi * self.freq * i / SAMPLE_RATE))
        return 1.0 - (d * v)


class SampleServer(BaseHTTPRequestHandler):
    def do_GET(self):
        size = int(SAMPLE_RATE * anywhere_between(8, 20))
        envelope = Amplitude(size)
        am = AmplitudeModulator(Constant(anywhere_between(0.3, 0.9)))
        note = int(parse_qs(urlparse(self.path).query)["note"][0])
        buff = [am.at(i) * envelope.at(i) * math.sin(2 * math.pi * freq(note) * i / SAMPLE_RATE) for i in range(size)]
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", size * 4)
        self.end_headers()
        self.wfile.write(struct.pack(f"{size}f", *buff))
        sys.stdout.write(f"generated {size} samples for note {note}\n\r")
        return None


HTTPServer(("0.0.0.0", 9965), SampleServer).serve_forever()
