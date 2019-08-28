#!/usr/bin/env python

import sounddevice as sd
import soundfile as sf
import sys
import math


filename = sys.argv[1]
(data, fs) = sf.read(filename, dtype="float32")

sd.play(data, fs)
sd.wait()
