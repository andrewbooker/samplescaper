#!/usr/bin/env python

import sys
import math


class LevelMonitor():
    def __init__(self):
        self.scale = 50
        self.height = 3    
        self.refresh = 10
        self.r = 0
        self.started = False
        self.recording = False
        
    def addBuffer(self, b):
        self.recording = b[0] > 0.4

        if self.r == self.refresh:
            if self.started:
                sys.stdout.write("\x1b[A" * self.height)
            for h in range(self.height):
                bar = int(self.scale * b[0])           
                sys.stdout.write("\033[92m%s\033[0m" % ("\u2589" * bar))
                sys.stdout.write("\u2591" * (self.scale - bar))
                sys.stdout.write("\033[96m%s\033[0m" % (("\u2588" if self.recording else " ") * 10))
                sys.stdout.write("\n\r")
    
            sys.stdout.flush()
            self.r = 0
            self.started = True
        else:
            self.r += 1




