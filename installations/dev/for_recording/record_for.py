#!/usr/bin/env python

import os
import sys
import time
from datetime import datetime, timedelta
import subprocess

start = datetime.now()
fileTs = start.strftime("%Y%m%d_%H%M%S")
fileName = f"~/Music/recording/randomatones_{fileTs}.wav"
#recCmd = f"arecord -f S16_LE -r 44100 --device=hw:1,0 -c1 {fileName} 2>/dev/null"
recCmd = f"ffmpeg -f alsa -channels 2 -sample_rate 44100 -i loopout {fileName} 2>/dev/null"
tailTime = 15

print("starting recording now at", start)
durSecs = int(sys.argv[1])
if durSecs < (tailTime + 5):
    print(f"min time {tailTime + 5}s")
    exit(0)

endTime = start + timedelta(0, durSecs)
stopPlayAfter = start + timedelta(0, durSecs - tailTime)
print("ending at", endTime)
proc = subprocess.Popen([recCmd], shell=True)
print("recording", proc.pid)
print("resuming")
os.system("tmux send-keys -t 1 r")
done = False
while not done:
    time.sleep(1)
    done = datetime.now() > stopPlayAfter

print("pausing")
os.system("tmux send-keys -t 1 p")
time.sleep(tailTime)
proc.terminate()
print("ended at", datetime.now())

