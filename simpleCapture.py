#!/usr/bin/env python

import keyboard
import time
import os
import sys
parentDir = os.path.dirname(os.getcwd())
sys.path.append(parentDir)
def checkImport(lib):
    if not os.path.exists(os.path.join(parentDir, lib)):
        print("%s library not found." % lib)
        print("please clone github.com/andrewbooker/%s.git into %s" % (lib, parentDir))
        exit()

checkImport("mediautils")
from mediautils.audiodevices import UsbAudioDevices

from capture.sampleRecording import Controller

if len(sys.argv) < 2:
    print("please supply output directory (will be created if necessary)")
    exit()

outDir = sys.argv[1]

if not os.path.exists(outDir):
    os.makedirs(outDir)
 

devices = UsbAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using", devices[audioDevice])
startNumber = int(sys.argv[2]) if len(sys.argv) > 2 else 48

controller = Controller(startNumber, audioDevice, outDir)

keyboard.on_press_key("q", controller.stopCapture, suppress = True)
keyboard.on_press_key("n", controller.incrementNumber, suppress = True)
keyboard.on_press_key("s", controller.toggleRecord, suppress = True)

while not controller.shouldStop.is_set():
    time.sleep(1)

print("done")
