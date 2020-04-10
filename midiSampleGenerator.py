#!/usr/bin/env python

import pygame.midi as midi
import time


def getUsableOutputDevice():
	c = midi.get_count()
	for d in range(c):
		info = midi.get_device_info(d)
		isOutput = (info[3] == 1)
		if isOutput and "USB" in str(info[1]):
			return d
	return None

midi.init()
device = getUsableOutputDevice()

if device is None:
	print("No USB MIDI devices found")
	midi.quit()
	exit()
	
player = midi.Output(device, latency = 0)

baseNote = 48
for i in range(24):
	note = baseNote + i
	player.note_on(note, velocity=100, channel=0)
	time.sleep(2.0)
	player.note_off(note, velocity=0, channel=0)
	time.sleep(1.0)

player.close()

del player
midi.quit()
