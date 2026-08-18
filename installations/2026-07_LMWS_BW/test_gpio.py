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
        self.stopStates = [0, 0]


    def run(self, shouldStop, shouldPause):
        while not shouldStop.is_set():
            s0 = GPIO.input(self.stopOut)
            s1 = GPIO.input(self.stopReturn)
            if s0 != self.stopStates[0]:
                print("stop out =", s0, f"({"light" if s0 == 0 else "dark"})")
                self.stopStates[0] = s0
            if s1 != self.stopStates[1]:
                print("stop return =", s1, f"{"light" if s1 == 0 else "dark"}")
                self.stopStates[1] = s1
            time.sleep(0.5)


ports = Ports()
controlPorts = [
    (26, 20, 7, 8),
#    (19, 16, 9, 10),
#    (6, 12, 24, 23)
]
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
