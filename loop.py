#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math


filename = sys.argv[1]
(data, fs) = sf.read(filename, dtype="float32")

sampleCount = len(data)

stereo = []
for i in range(sampleCount):
	left = data[i] * 0.9
	right = data[i] * 0.1
	stereo.append([left, right])

sd.play(stereo, fs)
sd.wait()
