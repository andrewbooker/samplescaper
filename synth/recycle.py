#!/usr/bin/env python

import random
import sys
import os
import time


def nextAudioFileFrom(poolDir):
    files = [f for f in filter(lambda f: "wav" in f and "_lin_" not in f and "__" not in f, os.listdir(poolDir))]
    if len(files) == 0:
        return None
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])

inDir = sys.argv[1]
outDir = sys.argv[2]
interval = float(sys.argv[3]) if len(sys.argv) > 3 else (10.0 + (40 * random.random()))
    
while True:
    f = nextAudioFileFrom(inDir)
    if f is not None:
        print("copying", f, "to", outDir)
        os.system("cp %s %s" % (f, outDir))
        time.sleep(interval)
