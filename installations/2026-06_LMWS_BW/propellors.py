#!/usr/bin/env python

import RPi.GPIO as GPIO
import random
import math
import json
import threading
import sys
import time


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


controlPorts = [(26, 20)]
motors = [Motors(c) for c in controlPorts]


class SingleUnit:
    def __init__(self, fwd, rev):
        self.fwd = fwd
        self.rev = rev
        self.interval = 1
        GPIO.output(self.fwd, 0)
        GPIO.output(self.rev, 0)

    def _move(self, out, back):
        GPIO.output(out, 1)
        time.sleep(self.interval)
        GPIO.output(out, 0)
        time.sleep(0.1)
        GPIO.output(back, 1)
        time.sleep(self.interval)
        GPIO.output(back, 0)
        time.sleep(0.1)

    def oneCycle(self):
        self._move(self.fwd, self.rev)
        self._move(self.rev, self.fwd)



threads = []
#threads.append(threading.Thread(target=startServer, args=(), daemon=True))

[t.start() for t in threads]
[t.join() for t in threads]

print("starting")
SingleUnit(*controlPorts[0]).oneCycle()
print("done")
