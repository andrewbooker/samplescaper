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


class LinearRampUpDown(Envelope):
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


class CosineAttenuation(Envelope):
    def __init__(self, freq_envelope, depth_envelope):
        self.freq = freq_envelope
        self.depth = depth_envelope

    def at(self, i):
        d = self.depth.at(i)
        v = 0.5 * (1.0 + math.cos(2 * math.pi * self.freq.at(i) * i / SAMPLE_RATE))
        return 1.0 - (d * v)


class SineVariation(Envelope):
    def __init__(self, freq_envelope, output_range):
        self.output_range = output_range
        self.freq = freq_envelope

    def at(self, i):
        l, u = self.output_range
        v = (u - l) * 0.5 * (1.0 + math.sin(2 * math.pi * self.freq.at(i) * i / SAMPLE_RATE))
        return l + v


class Modulation(Envelope):
    def __init__(self, central_value, f, depth):
        self.c = central_value
        self.d = depth
        self.f = f

    def at(self, i):
        return self.c + (self.d * (1.0 + math.sin(2 * math.pi * self.f * i / SAMPLE_RATE)))


class SampleServer(BaseHTTPRequestHandler):
    def do_GET(self):
        size = int(SAMPLE_RATE * anywhere_between(8, 20))
        note = int(parse_qs(urlparse(self.path).query)["note"][0])
        envelope = LinearRampUpDown(size)
        mf = Modulation(freq(note), anywhere_between(0.1, 2.1), anywhere_between(0.01, 0.08))
        am_depth = SineVariation(Constant(anywhere_between(0.1, 10.0)), (anywhere_between(0.005, 0.3), anywhere_between(0.4, 1.0)))
        am_freq = Constant(anywhere_between(0.3, 7.0))
        am = CosineAttenuation(am_freq, am_depth)
        buff = [am.at(i) * envelope.at(i) * math.sin(2 * math.pi * mf.at(i) * i / SAMPLE_RATE) for i in range(size)]
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", size * 4)
        self.end_headers()
        self.wfile.write(struct.pack(f"{size}f", *buff))
        sys.stdout.write(f"generated {size} samples for note {note}\n\r")
        return None


HTTPServer(("0.0.0.0", 9965), SampleServer).serve_forever()
