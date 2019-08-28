#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math


filename = sys.argv[1]
(data, fs) = sf.read(filename, dtype="float32")

sampleCount = len(data)
ramp = math.floor(sampleCount / 5.0)


stereo = []
for i in range(sampleCount):
	sample = data[i]
	if (i < ramp):
		sample *= (1.0 * i / ramp)

	if (i > (sampleCount - ramp)):
		sample *= (1.0 * (sampleCount - i) / ramp)

	left = sample * 0.9
	right = sample * 0.1
	stereo.append([left, right])

sd.play(stereo, fs)
sd.wait()
