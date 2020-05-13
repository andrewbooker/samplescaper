#!/usr/bin/env python

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
from mediautils.mididevices import UsbMidiDevices, MidiOut

midiDevices = UsbMidiDevices()
midiOut = MidiOut(midiDevices)
baseNote = 52
channel = int(sys.argv[1]) - 1 if len(sys.argv) > 1 else 0

for i in range(5):
    note = baseNote + (i * 7)
    midiOut.io.note_on(note, velocity=100, channel=channel)
    time.sleep(0.1)
    midiOut.io.note_off(note, velocity=0, channel=channel)
    time.sleep(0.9)

del midiOut
del midiDevices
