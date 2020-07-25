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

    def start(self, shouldStop):
        v = Value("d", 0.0)
        started = False
        while not shouldStop.is_set():
            with v.get_lock():
                v.value = self.lastValue

            self.recording = v.value > 0.1
            if started:
                sys.stdout.write("\x1b[A" * self.height)

            bar = int(self.scale * v.value)
            fill = self.scale - bar
            for h in range(self.height):
                sys.stdout.write("\033[92m%s\033[0m" % ("\u2589" * bar))
                sys.stdout.write("\u2591" * fill)
                sys.stdout.write("\033[96m%s\033[0m" % (("\u2588" if self.recording else " ") * 10))
                sys.stdout.write("\n\r")

            sys.stdout.flush()
            started = True
            time.sleep(self.refreshMillis / 1000.0)

    def addBuffer(self, b):
        self.lastValue = abs(b[0])





