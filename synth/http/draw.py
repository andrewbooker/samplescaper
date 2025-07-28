#!/usr/bin/python3

import sys
import struct
from PIL import Image, ImageDraw

WIDTH = 882
SMPL = 44100

data = []
with open(sys.argv[1], "rb") as in_file:
    in_data = in_file.read()
    for i in range(SMPL):
        p = i * 4
        data.extend(struct.unpack('f', in_data[p:p + 4]));

out = Image.new("RGB", (WIDTH, 200), (0, 0, 0))

d = ImageDraw.Draw(out)
for i in range(0, SMPL):
    d.point((i * 1.0 * WIDTH / SMPL, 100 + (100 * data[i])), fill="red")

out.show()
