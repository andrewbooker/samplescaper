import os
import re
from subprocess import PIPE, Popen

class SystemVolume():
    PERCENT = r"\[\d{1,2}%\]"
    VALUE = r"\d+"

    @staticmethod
    def read(deviceName):
        p = Popen("amixer sget '%s'" % deviceName, shell=True, stdout=PIPE, stderr=PIPE)
        (o, e) = p.communicate()
        t = o.decode()
        if "Unable to find simple control" in e.decode() or "Playback channels: Mono" in t:
            return None
        return t

    def __init__(self):
        self.deviceName = None
        for d in ["Digital", "Master"]:
            if SystemVolume.read(d) is not None:
                self.deviceName = d
                print("found audio device '%s'" % d)
                return

    def get(self):
        if self.deviceName is None:
            return []
        c = SystemVolume.read(self.deviceName)
        if c is None:
            return []
        v = [t for t in filter(lambda t: "[" in t, c.split("\n"))]
        r = map(lambda t: re.search(SystemVolume.PERCENT, t).group(), v)
        return [int(re.search(SystemVolume.VALUE, v).group()) for v in r]

    def set(self, l, r):
        os.system("amixer sset '%s' %d%%,%d%%" % (self.deviceName, l, r))

