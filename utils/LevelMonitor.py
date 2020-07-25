#!/usr/bin/env python

import sys
import math
import time
from multiprocessing import Value


class LevelMonitor():
    def __init__(self):
        self.scale = 50
        self.height = 3    
        self.refreshMillis = 100
        self.recording = False
        self.lastValue = 0.0
        self.msg = ""

    def start(self, shouldStop):
        v = Value("d", 0.0)
        started = False
        while not shouldStop.is_set():
            with v.get_lock():
                v.value = self.lastValue

            if started:
                sys.stdout.write("\x1b[A" * (self.height + 1))

            bar = int(self.scale * v.value)
            fill = self.scale - bar
            rec = ("\u2588" if self.recording else " ")
            for h in range(self.height):
                sys.stdout.write("\033[92m%s\033[0m" % ("\u2589" * bar))
                sys.stdout.write("\u2591" * fill)
                sys.stdout.write("\033[96m%s\033[0m" % (rec * 10))
                sys.stdout.write("\n\r")

            sys.stdout.write("%s\n\r" % self.msg)
            sys.stdout.flush()
            started = True
            time.sleep(self.refreshMillis / 1000.0)

    def setRecording(self, r):
        self.recording = r

    def setMessage(self, m):
        self.msg = m

    def addBuffer(self, b):
        self.lastValue = abs(b[0])





