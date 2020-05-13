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
from mediautils.audiodevices import UsbAudioDevices
from mediautils.mididevices import UsbMidiDevices, MidiOut

from utils.sampleRecording import Controller


#### same as simpleCapture
if len(sys.argv) < 2:
    print("please supply output directory (will be created if necessary)")
    exit()

outDir = sys.argv[1]

if not os.path.exists(outDir):
    os.makedirs(outDir)


devices = UsbAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using", devices[audioDevice])
baseNote = int(sys.argv[2]) if len(sys.argv) > 2 else 48
cycles = int(sys.argv[3]) if len(sys.argv) > 3 else 1


controller = Controller(baseNote, audioDevice, outDir)
####

midiDevices = UsbMidiDevices()
midiOut = MidiOut(midiDevices)

for c in range(cycles):
	for i in range(18):
		note = baseNote + i
		controller.setNumber(note)
		midiOut.io.note_on(note, velocity=100, channel=0)
		controller.toggleRecord(None)
		time.sleep(4.0)
		controller.toggleRecord(None)
		midiOut.io.note_off(note, velocity=0, channel=0)
		time.sleep(4.0)

controller.stopCapture(None)

del midiOut
del midiDevices
