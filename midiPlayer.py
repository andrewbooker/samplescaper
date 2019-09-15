#!/usr/bin/env python


import pygame.midi as midi
import time


midi.init()

i = 0
player = midi.Output(0)
while (i != 127):
	print("Playing %d" % i)
	player.set_instrument(i) # a program change
	player.note_on(64, 127, 0)
	time.sleep(2)
	player.note_off(64, 127, 0)
	i += 1

del player

midi.quit()