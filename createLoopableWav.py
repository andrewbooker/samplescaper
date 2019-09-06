#!/usr/bin/env python


import soundfile as sf
import sys
import math

class RawSample():
    def __init__(self, file):
        print("loading %s" % file)
        (data, ignore) = sf.read(file, dtype="float32")
        self.data = data
		
		
def createLoopableSample(raw):
	halfWay = math.floor(len(raw.data) / 2)
	xFade = math.floor(halfWay / 2)
	data = []
	
	for s in range(len(raw.data) - xFade):
		p = s + xFade - halfWay
		if s < (halfWay - xFade):
			data.append(raw.data[s + halfWay])
		elif s >= halfWay:
			data.append(raw.data[p])
		else:
			f = 1.0 * p / xFade
			data.append((f * raw.data[p]) + ((1.0 - f) * raw.data[s + halfWay])) 
			
	return data

rawSample = RawSample(sys.argv[1])
sf.write("testLoop.wav", createLoopableSample(rawSample), 44100)
	
	