#!/usr/bin/env python

import os
import sys
import shutil

inDir = sys.argv[1]
recordings = [f for f in filter(lambda f: f.startswith("20"), os.listdir(inDir))]
recordings.sort()
toDelete = recordings[:-3]
print("found", len(toDelete), "folder(s) to remove")
for r in toDelete:
    print("dropping", r)
    shutil.rmtree(os.path.join(inDir, r))

