#!/usr/bin/env python

import pygame.midi as midi
import time
import sys
import os

from utils.sampleRecording import Controller
from utils.sampleRecording import usableAudioDevices


def getUsableOutputDevice():
	c = midi.get_count()
	for d in range(c):
		info = midi.get_device_info(d)
		isOutput = (info[3] == 1)
		if isOutput and "USB" in str(info[1]):
			return d
	return None

#### same as simpleCapture
if len(sys.argv) < 2:
    print("please supply output directory (will be created if necessary)")
    exit()

outDir = sys.argv[1]

if not os.path.exists(outDir):
    os.makedirs(outDir)


devices = usableAudioDevices()
audioDevice = [k for k in devices.keys()][0]
print("using %s" % devices[audioDevice])
baseNote = int(sys.argv[2]) if len(sys.argv) > 2 else 48
cycles = int(sys.argv[3]) if len(sys.argv) > 3 else 1


controller = Controller(baseNote, audioDevice, outDir)
####

midi.init()
device = getUsableOutputDevice()

if device is None:
	print("No USB MIDI devices found")
	midi.quit()
	exit()
	
player = midi.Output(device, latency = 0)

for c in range(cycles):
	for i in range(18):
		note = baseNote + i
		controller.setNumber(note)
		player.note_on(note, velocity=100, channel=0)
		controller.toggleRecord(None)
		time.sleep(4.0)
		controller.toggleRecord(None)
		player.note_off(note, velocity=0, channel=0)
		time.sleep(4.0)

controller.stopCapture(None)
player.close()

del player
midi.quit()
