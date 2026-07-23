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


class OnOffDir:
    def __init__(self, onOff, direction, ports):
        self.onOff = onOff
        self.direction = direction
        ports.newOutput(onOff)
        ports.newOutput(direction)
        self.interval = 1.0
        GPIO.output(self.onOff, 0)
        GPIO.output(self.direction, 0)

    def oneCycle(self):
        print("starting cycle")
        GPIO.output(self.onOff, 1)
        GPIO.output(self.direction, 1)
        time.sleep(self.interval)
        GPIO.output(self.direction, 0)
        time.sleep(self.interval * 2)
        GPIO.output(self.direction, 1)
        time.sleep(self.interval)
        GPIO.output(self.onOff, 0)
        time.sleep(0.1)
        print("cycle done")


    def run(self, shouldStop, shouldPause):
        while not shouldStop.is_set():
            if shouldPause.is_set():
                time.sleep(1)
            else:
                time.sleep(1.5 * random.random())
                self.oneCycle()
                time.sleep(3.0 * random.random())


ports = Ports()
controlPorts = [(26, 20), (19, 16), (6, 12)]
units = [OnOffDir(*c, ports) for c in controlPorts]
shouldStop = threading.Event()
shouldPause = threading.Event()
threads = [threading.Thread(target=u.run, args=(shouldStop, shouldPause), daemon=True) for u in units]

print("starting (press 'q' to exit)")
[t.start() for t in threads]
while not shouldStop.is_set():
    c = readchar.readchar()
    if c == "q":
        shouldStop.set()
    if c == "p" and not shouldPause.is_set():
        shouldPause.set()
        print("paused")
    if c == "r" and shouldPause.is_set():
        shouldPause.clear()
        print("resuming")

print("stopping")
[t.join() for t in threads]
print("done")
