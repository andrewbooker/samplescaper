#!/usr/bin/env python

import sys
from os import listdir
import utils.scale as modal
from os.path import isfile, join

path = sys.argv[1]
root = sys.argv[2] if len(sys.argv) > 2 else "E"
mode = sys.argv[3] if len(sys.argv) > 3 else "aeolian"
noteRange = [int(n) for n in listdir(path)]

print("range available")
print(noteRange)

modalNotes = modal.Scale(7, modal.roots[root], modal.modes[mode]).notes

def searchOctave(i, o):
    n = modalNotes[i] + (o * 12)
    if n in noteRange:
        modalNotes.append(n)

for i in range(7):
	searchOctave(i, -2)
	searchOctave(i, -1)
	searchOctave(i, 1)
	searchOctave(i, 2)
		
scale = [n for n in modalNotes if n in noteRange]
print("%d notes available in %s %s" % (len(scale), root, mode))
print(scale)


files = {}
for s in scale:
    notePath = join(path, str(s))
    files[s] = [join(notePath, f) for f in listdir(notePath) if isfile(join(notePath, f))]

