#!/usr/bin/env python

import RPi.GPIO as GPIO
import random
import math
from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import threading
import sys


class Ports():
    def __init__(self):
        print("initialising GPIO")
        GPIO.setmode(GPIO.BCM)
        self.ports = []

    def __del__(self):
        print("cleaning up GPIO ports")
        [p.stop() for p in self.ports]
        GPIO.cleanup()

    def newOutput(self, channel):
        GPIO.setup(channel, GPIO.OUT, initial=0)


class Motors():
    def __init__(self, pins):
        self.ports = Ports()
        self.pins = {p: 0 for p in pins}
        for p in pins:
            self.ports.newOutput(p)

    def on(self, pin):
        GPIO.output(pin, 1)
        self.pins[pin] = 1

    def off(self, pin):
        GPIO.output(pin, 0)
        self.pins[pin] = 0


channelMapping = {
    0: 6,
    1: 12
}
motors = Motors([v for _, v in channelMapping.items()])
PORT = 9971


class Controller(BaseHTTPRequestHandler):
    def __standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def do_OPTIONS(self):
        self.__standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, POST")
        self.end_headers()

    def do_GET(self):
        self.__standardResponse()
        self.end_headers()
        payload = {p: v for p, v in motors.pins.items()}
        self.wfile.write(json.dumps(payload).encode("utf-8"))

    def _stop(self, idx):
        motors.off(idx)

    def _start(self, idx):
        motors.on(idx)

    def do_POST(self):
        ps = self.path.split("?")
        p = ps[0][1:]
        q = ps[1]
        idx = int(q)
        port = channelMapping[idx]
        getattr(self, f"_{p}")(port)
        self.do_GET()


def startServer():
    HTTPServer(("0.0.0.0", PORT), Controller).serve_forever()


threads = []
threads.append(threading.Thread(target=startServer, args=(), daemon=True))

[t.start() for t in threads]
print("serving on port", PORT)

[t.join() for t in threads]
del ports
