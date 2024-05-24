import os
import re
from subprocess import PIPE, Popen

from processLogging import createLog
log = createLog(__name__)

class SystemVolume():
    PERCENT = r"\[\d{1,2}%\]"
    VALUE = r"\d+"

    @staticmethod
    def read(deviceName):
        p = Popen("amixer sget '%s'" % deviceName, shell=True, stdout=PIPE, stderr=PIPE)
        (o, e) = p.communicate()
        t = o.decode()
        if "Unable to find simple control" in e.decode() or "Playback channels: Mono" in t:
            log.info(t)
            log.info(e.decode())
            return None
        return t

    @staticmethod
    def parse_percentage(t):
        pc = re.search(SystemVolume.PERCENT, t)
        if pc is None:
            return "0"
        return pc.group()

    def _device(self):
        if self.deviceName is None:
            for d in ["Digital", "Master"]:
                if SystemVolume.read(d) is not None:
                    self.deviceName = d
                    log.info(f"found audio device '{d}'")

            if self.deviceName is None:
                log.info("Unable to find audio device")
        return self.deviceName

    def __init__(self):
        self.deviceName = None

    def get(self):
        dev = self._device()
        if dev is None:
            log.info("No audio device known when trying to read current volume")
            return []
        c = SystemVolume.read(dev)
        if c is None:
            return []
        v = [t for t in filter(lambda t: "[" in t, c.split("\n"))]
        r = map(SystemVolume.parse_percentage, v)
        return [int(re.search(SystemVolume.VALUE, v).group()) for v in r]

    def set(self, l, r):
        dev = self._device()
        if dev is not None:
            os.system("amixer sset '%s' %d%%,%d%%" % (dev, l, r))

