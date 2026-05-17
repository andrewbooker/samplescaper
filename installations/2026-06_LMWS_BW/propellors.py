#!/usr/bin/env python

import RPi.GPIO as GPIO
import random
import threading
import sys
import time
import readchar

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


class SingleUnit:
    def __init__(self, fwd, rev, ports):
        self.fwd = fwd
        self.rev = rev
        ports.newOutput(fwd)
        ports.newOutput(rev)
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

    def run(self, shouldStop):
        while not shouldStop.is_set():
            self.oneCycle()
            time.sleep(3.0 * random.random())


ports = Ports()
controlPorts = [(26, 20)]
units = [SingleUnit(*c, ports) for c in controlPorts]
shouldStop = threading.Event()

threads = [threading.Thread(target=u.run, args=(shouldStop,), daemon=True) for u in units]

print("starting (press 'q' to exit)")
[t.start() for t in threads]
while not shouldStop.is_set():
    c = readchar.readchar()
    if c == "q":
        shouldStop.set()

print("stopping")
[t.join() for t in threads]
print("done")
