#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math

import random


class RawSample():
	def __init__(self, file):
		(data, sampleRate) = sf.read(file, dtype="float32")
		self.data = data


class Sample():
	def __init__(self, data):
		self.pan = random.random()
		sampleCount = len(data)
		ramp = math.floor(sampleCount / (6.0 * random.random()))
		
		self.stereo = []
		for i in range(sampleCount):
			sample = data[i]
			if (i < ramp):
				sample *= (1.0 * i / ramp)

			if (i > (sampleCount - ramp)):
				sample *= (1.0 * (sampleCount - i) / ramp)

			left = sample * self.pan
			right = sample * (1.0 - self.pan)
			self.stereo.append([left, right])
	
file = RawSample(sys.argv[1])
sample1 = Sample(file.data)
sd.play(sample1.stereo, 44100)
sd.wait()
print("done")
	


