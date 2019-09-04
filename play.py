#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math

import random
import queue

sd.default.samplerate = 44100
sd.default.channels = 2


class RawSample():
	def __init__(self, file):
		(data, ignore) = sf.read(file, dtype="float32")
		self.data = data


class Sample():
	def __init__(self, data):
		self.data = data
		self.pan = random.random()
		self.pos = 0
		self.sampleCount = len(data)
		self.ramp = math.floor(self.sampleCount / (6.0 * random.random()))
		self.rampDown = self.sampleCount - self.ramp

	def hasData(self):
		return self.pos < self.sampleCount
		
	def read(self, n):
		out = []
		for _ in range(n):
			sample = self.data[self.pos] if self.hasData() else 0
			if (self.pos < self.ramp):
				sample *= (1.0 * self.pos / self.ramp)

			if (self.hasData() and (self.pos > self.rampDown)):
				sample *= (1.0 - ((self.pos - self.rampDown) / (1.0 * self.ramp)))

			left = sample * self.pan
			right = sample * (1.0 - self.pan)

			out.append([left, right])
			self.pos += 1
		return out
	
file = RawSample(sys.argv[1])
sample1 = Sample(file.data)

q = queue.Queue()

def callback(outdata, frames, time, status):
    outdata[:] = q.get()
	
def finishedPlaying():
    print("finished")

blocksize = 1000
q.put(sample1.read(blocksize))
stream = sd.OutputStream(blocksize=blocksize, dtype="float32", callback=callback, finished_callback =finishedPlaying)


with stream:
    while sample1.hasData():
        q.put(sample1.read(blocksize))
    print("finished reading")
