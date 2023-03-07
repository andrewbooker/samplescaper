#!/usr/bin/env python

import os
import re

class Volume():
    PERCENT = r"\[\d{2}%\]"
    VALUE = r"\d+"

    @staticmethod
    def find(deviceName):
        t = os.popen("amixer sget '%s'" % deviceName).read()
        if "Unable to find simple control" in t or "Playback channels: Mono" in t:
            return None
        return t

    def __init__(self):
        self.deviceName = None
        for d in ["Digital", "Master"]:
            if Volume.find(d) is not None:
                self.deviceName = d
                return

    def get(self):
        if self.deviceName is None:
            return []
        c = Volume.find(self.deviceName)
        if c is None:
            return []
        v = [t for t in filter(lambda t: "[" in t, c.split("\n"))]
        r = map(lambda t: re.search(Volume.PERCENT, t).group(), v)
        return [int(re.search(Volume.VALUE, v).group()) for v in r]


print(Volume().get())
