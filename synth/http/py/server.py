#!/usr/bin/env python

import math
import random
import struct
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs


if len(sys.argv) < 2:
    print("Must supply port number")
    exit(0)


PORT = int(sys.argv[1])
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


class HalfCosineRamp(Envelope):
    def __init__(self, over):
        self.over = over
        self.ramp_up = int(SAMPLE_RATE * anywhere_between(2.0, 4.0))
        self.ramp_down = int(anywhere_between(0.2 * over, 0.5 * over))
        self.start_ramp_down = self.over - self.ramp_down

    def at(self, i):
        if i < self.ramp_up:
            return 0.5 * (1.0 + math.cos(math.pi * (i + self.ramp_up) / self.ramp_up))
        if i > self.start_ramp_down:
            return 0.5 * (1.0 + math.cos(math.pi * (i - self.start_ramp_down) / self.ramp_down))
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


class Compressor:
    def __init__(self, gain_envelope, threshold_envelope):
        self.gain = gain_envelope
        self.threshold = threshold_envelope

    def apply_to(self, i, v):
        gain = self.gain.at(i)
        threshold = self.threshold.at(i)
        s = v * gain
        if abs(s) < threshold:
            return s

        r = threshold + ((abs(s) - threshold) * (1.0 - threshold) / (gain - threshold))
        return -r if s < 0 else r


def envelope(from_val, to_val, note_freq = None):
    v = anywhere_between(from_val, to_val)
    if random.random() > 0.3:
        return Constant(v)

    if random.random() > 0.8 and note_freq is not None:
        return SineVariation(Constant(anywhere_between(0.01 * note_freq, note_freq)), (from_val, to_val))

    return SineVariation(Constant(anywhere_between(0.01, 4.0)), (from_val, to_val))


class SampleServer(BaseHTTPRequestHandler):
    def do_GET(self):
        size = int(SAMPLE_RATE * anywhere_between(8, 20))
        note = int(parse_qs(urlparse(self.path).query)["note"][0])
        if random.random() > 0.7:
            note += 12
        if random.random() > 0.7:
            note += 12
        f = freq(note)
        sys.stdout.write(f"generating {note} at {f:.4f}Hz for {size / SAMPLE_RATE:.4f}s\n\r")
        vol_coeff = 1.0 if note < 60 else 1.0 - (0.5 * (note - 60) / 35)

        ramp_up_down = HalfCosineRamp(size)
        mf = Modulation(f, anywhere_between(0.01, 1.0), anywhere_between(0.01, 0.08))

        am_depth = envelope(0.1, 0.9)
        am_freq = envelope(0.3, 7.0, f)
        am = CosineAttenuation(am_freq, am_depth)

        phase = envelope(anywhere_between(-0.008 * f, 0.0001 * f), anywhere_between(0.0001 * f, 0.008 * f), f)

        cmp_gain = envelope(anywhere_between(1.0, 1.2), anywhere_between(1.3, 10), f)
        cmp_threshold = envelope(anywhere_between(0.2, 0.5), anywhere_between(0.51, 0.9))
        compressor = Compressor(cmp_gain, cmp_threshold)

        buff = [vol_coeff * am.at(i) * ramp_up_down.at(i) * compressor.apply_to(i, math.sin(phase.at(i) + (2 * math.pi * mf.at(i)) * i / SAMPLE_RATE)) for i in range(size)]
        self.send_response(200)
        self.send_header("Content-Type", "application/octet-stream")
        self.send_header("Content-Length", size * 4)
        self.end_headers()
        self.wfile.write(struct.pack(f"{size}f", *buff))
        return None


HTTPServer(("0.0.0.0", PORT), SampleServer).serve_forever()
