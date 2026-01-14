#!/usr/bin/env python

import threading
import readchar
from LevelMonitor import LevelMonitor

level = LevelMonitor()

shouldStop = threading.Event()

threads = []
threads.append(threading.Thread(target=level.start, args=(shouldStop,), daemon=True))

[t.start() for t in threads]
done = False

print("Started. Press 'q' to exit")
while not done:
    c = readchar.readchar()
    if c == "q":
        shouldStop.set()
        done = True
    elif c == 'h':
        level.setMessage("Hello")
    elif c == 'g':
        level.setMessage("Goodbye")
    elif c == 'l':
        level.addBuffer([0.5])
    elif c == 'e':
        level.setRecording(1)
    elif c == 'c':
        level.setRecording(2)
    elif c == 's':
        level.setRecording(0)

[t.join() for t in threads]

