#!/usr/bin/env python

import sys
import os
import keyboard
import time

from utils.sampleRecording import Controller
from utils.sampleRecording import usableAudioDevices

if len(sys.argv) < 2:
    print("please supply output directory (will be created if necessary)")
    exit()

outDir = sys.argv[1]

if not os.path.exists(outDir):
    os.makedirs(outDir)
 

devices = usableAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using %s" % devices[audioDevice])
startNumber = int(sys.argv[2]) if len(sys.argv) > 2 else 48

controller = Controller(startNumber, audioDevice, outDir)

keyboard.on_press_key("q", controller.stopCapture, suppress = True)
keyboard.on_press_key("n", controller.incrementNumber, suppress = True)
keyboard.on_press_key("s", controller.toggleRecord, suppress = True)

while not controller.shouldStop.is_set():
    time.sleep(1)

print("done")
