#!/usr/bin/env python


import pygame.midi as midi
import time
import threading


midi.init()


class Instrument():
	def __init__(self, program, player, channel):
		self.channel = channel
		self.player = player
		self.player.set_instrument(program) # a program change
		
	def play(self, note, vel, durSecs):
		self.player.note_on(note, vel, self.channel)
		time.sleep(durSecs)
		self.player.note_off(note, vel, self.channel)
		
		

def playNotes(ins, n, interval, shouldStop):
	for i in range(24):
		ins.play(n + (2 * i), 127, 0.2 * interval)
	shouldStop.set()

stopped1 = threading.Event()
stopped2 = threading.Event()
player = midi.Output(0)

i1 = Instrument(8, player, 0)
i2 = Instrument(10, player, 1)

t1 = threading.Thread(target = playNotes, args = (i1, 60, 1, stopped1), daemon=True)
t2 = threading.Thread(target = playNotes, args = (i2, 52, 1.5, stopped2), daemon=True)
t1.start()
t2.start()

t1.join()
t2.join()

while not stopped1.is_set() and not stopped2.is_set():
	time.sleep(1)

del i1
del i2
del player
midi.quit()