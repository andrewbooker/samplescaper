#!/usr/bin/env python

roots = {"A":  57,
		 "A#": 58,
		 "Bb": 58,
		 "B":  59,
		 "C":  60,
		 "C#": 61,
		 "Db": 61,
		 "D":  62,
		 "D#": 63,
		 "Eb": 63,
		 "E":  64, 
		 "F":  65,
		 "F#": 66,
		 "Gb": 66,
		 "G":  67,
		 "G#": 68,
		 "Ab": 68,
		 "A":  69}

modes = {"aeolian": [2, 1, 2, 2, 1, 2],
		 "dorian": [2, 1, 2, 2, 2, 1],
		 "ionian": [2, 2, 1, 2, 2, 2],
		 "mixolydian": [2, 2, 1, 2, 2, 1],
		 "lydian": [2, 2, 2, 1, 2, 2],
		 "eastern": [1, 3, 1, 2, 1, 2],
		 "diminished": [3, 3, 3],
		 "wholetone": [2, 2, 2, 2, 2],
		 "chromatic":[1, 1, 1, 1, 1, 1, 1, 1, 1, 1]}

class Scale():
	def __init__(self, chordSize, root, mode):
		octaves = -1
		base = root
		self.root = root
		self.notes = []
		for n in range(chordSize):
			if ((n % (len(mode) + 1)) == 0):
				octaves += 1
				base = root + (octaves * 12)
			else:
				base += mode[(n - (1 + octaves)) % len(mode)]
			
			self.notes.append(base)

	def note(self, i):
		return self.notes[i]
		
