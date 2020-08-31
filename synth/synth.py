#!/usr/bin/env python

import math
import soundfile as sf
import random

def freq(n):
    return math.pow(2, (n - 69)/12.0) * 440

def anywhereBetween(v1, v2):
    return v1 + (random.random() * random.random() * random.random() * (v2 - v1))

def genQuadrants():
    nodes = [1.0, 0.0, -1.0, 0.0]
    q = []
    r = 1.0
    for v in range(len(nodes) - 1):
        l = 0.5 * r * random.random()
        q.append((l, nodes[v]))
        r -= l

    q.append((r, nodes[-1]))
    return q

def genRandomTemplateFrom(quadrants):
    q = 0
    qp = 0.0
    val = 0.0

    template = []
    templateLength = 256

    for i in range(templateLength):
        quadrant = quadrants[q];

        val = anywhereBetween(val, quadrant[1])
        template.append(val)

        qp += 1.0 / templateLength
        if qp > quadrant[0]:
            val = quadrant[1] 
            q += 1
            qp = 0.0

    return template

sampleRate = 44100
quadrants = genQuadrants()
print(quadrants)
template = genRandomTemplateFrom(quadrants)
templateLength = len(template)


durSecs = 2
f = freq(67)
data = []
stretch = f * templateLength / sampleRate
print(stretch, f)

for i in range(sampleRate * durSecs):
    ps = math.modf(i * stretch)
    p = ps[0] + (int(ps[1]) % templateLength)
    p0 = math.floor(p)
    p1 = (p0 + 1) % templateLength
    v0 = template[p0]
    v1 = template[p1]
    data.append(((p - p0) * v1) + ((p1 - p) * v0))


sf.write("test.wav", data, sampleRate)
