#!/usr/bin/python

import RPi.GPIO as gpio
import time
import random
from http.server import HTTPServer, BaseHTTPRequestHandler
import json
import threading


class RgbSelector:
    def __init__(self):
        self.freq = 400
        self.port_numbers = [
            (23, 67), (24, 20), (25, 100), (8, 20)
        ]
        self.ports = []
        self.values = dict()
        gpio.setmode(gpio.BCM)
        for p in self.port_numbers:
            gpio.setup(p[0], gpio.OUT)
            self.ports.append((gpio.PWM(p[0], self.freq), p[1], p[0]))
        for p in self.ports:
            p[0].start(0)

    def __del__(self):
        print("shutting down")
        gpio.cleanup()

    def set_values(self):
        for p in self.ports:
            v = int((5 + (95 * random.random())) * p[1] / 100.0)
            self.values[p[2]] = v
            p[0].ChangeDutyCycle(v)


selector = RgbSelector()
PORT = 9988

class Controller(BaseHTTPRequestHandler):
    def __standardResponse(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def do_OPTIONS(self):
        self.__standardResponse()
        self.send_header("Access-Control-Allow-Methods", "GET, PUT")
        self.end_headers()

    def do_GET(self):
        self.__standardResponse()
        self.end_headers()
        payload = {
            "values": selector.values
        }
        self.wfile.write(json.dumps(payload).encode("utf-8"))

    def do_PUT(self):
        selector.set_values()
        self.do_GET()


def startServer():
    HTTPServer(("0.0.0.0", PORT), Controller).serve_forever()


threads = []
threads.append(threading.Thread(target=startServer, args=(), daemon=True))
[t.start() for t in threads]
print("serving on port", PORT)

[t.join() for t in threads]
del selector
