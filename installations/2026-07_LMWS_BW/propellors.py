#!/usr/bin/env python3

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

    def newInput(self, channel):
        GPIO.setup(channel, GPIO.IN, pull_up_down=GPIO.PUD_UP)


class OnOffDir:
    def __init__(self, onOff, direction, stopOut, stopReturn, ports):
        self.onOff = onOff
        self.direction = direction
        self.stopOut = stopOut
        self.stopReturn = stopReturn
        ports.newOutput(onOff)
        ports.newOutput(direction)
        ports.newInput(stopOut)
        ports.newInput(stopReturn)
        GPIO.output(self.onOff, 0)
        GPIO.output(self.direction, 0)

    def oneDirection(self, startDirection, stopPort):
        GPIO.output(self.direction, startDirection)
        GPIO.output(self.onOff, 1)
        i = 0
        while GPIO.input(stopPort) == 1 and i < 30
            time.sleep(0.1)
            i += 1
        GPIO.output(self.onOff, 0)
        time.sleep(1.5 * random.random())

    def oneCycle(self):
        print("starting cycle")
        self.oneDirection(1, self.stopOut)
        self.oneDirection(0, self.stopReturn)
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
controlPorts = [(26, 20, 0, 0), (19, 16, 0, 0), (6, 12, 0, 0)]
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
