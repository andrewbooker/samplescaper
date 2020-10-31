#!/usr/bin/env python

import soundfile as sf
import sys
import os
import random
import math
import time

def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])
	
def stretch():
	s = 0.1 + (2.1 * random.random())
	if s > 0.7 and s < 1.3:
		return stretch
	return s

inDir = os.path.join(sys.argv[1], "samples")
outDir = os.path.join(sys.argv[1], "modified")

while True:
	f = nextAudioFileFrom(inDir)
	print("using", os.path.basename(f))
	data, samplerate = sf.read(f)
	
	sd = len(data)
	out = []
	s = 0.1 + (2.1 * random.random())
	print(s)
	for i in range(int(sd * s)):
		p = i / s
		p0 = math.floor(p)
		p1 = math.ceil(p)
		if p1 >= sd:
			p1 = p0
		e = p - p0
		out.append(((1.0 - e) * data[p0]) + (e * data[p1]))

	sf.write(os.path.join(outDir, "%s_%.3f.wav" % (os.path.basename(f), s)), out, samplerate)
	time.sleep(1)
	