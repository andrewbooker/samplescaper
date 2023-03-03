
import os
import sys
import threading
import time
import datetime
import sys
import random
from pathlib import Path

random.seed()
    
class Player():
    def __init__(self, inDir, numberOfConcurrentSounds):
        self.poolDir = os.path.join(inDir, "looped")

        self.playedDir = os.path.join(Path(inDir).parent, datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H%M%S"))
        if not os.path.exists(self.playedDir):
            os.mkdir(self.playedDir)

        self.numberOfConcurrentSounds = numberOfConcurrentSounds
        self.shouldStop = threading.Event();
        self.threads = []
        self.controlThread = None

    def _nextAudioFile(self):
        files = [f for f in filter(lambda f: "wav" in f, os.listdir(self.poolDir))]
        if len(files) == 0:
            return None
        return os.path.join(self.poolDir, files[random.randint(0, len(files) - 1)])

    def _playOne(self, startedAt):
        f = self._nextAudioFile()
        if f is None:
            sys.stdout.write("%.6f: No files in %s\n\r" % (time.time(), self.poolDir))
            return

        with open(os.path.join(self.playedDir, "inventory.lof"), "a") as lof:
            lof.write("file \"%s\" offset %f\n" % (os.path.join(sys.argv[1], "looped", os.path.basename(f)), time.monotonic() - startedAt))

        os.system("aplay -q %s" % f)

        if not Path(os.path.join(self.playedDir, os.path.basename(f))).exists():
            if len(os.listdir(self.poolDir)) > 7:
                sys.stdout.write("%.6f: moving to %s to %s\n\r" % (time.monotonic(), f, self.playedDir))
                os.system("mv %s %s" % (f, self.playedDir))
            else:
                sys.stdout.write("%.6f: copying %s to %s\n\r" % (time.monotonic(), f, self.playedDir))
                os.system("cp %s %s" % (f, self.playedDir))
        else:
            sys.stdout.write("%.6f: already stored %s\n\r" % (time.monotonic(), f))
    
    def _playContinuously(self):
        self.threads.clear()
        startedAt = time.monotonic()
        start = time.time()
        print("starting at", start)
        while not self.shouldStop.is_set():
            if len(self.threads) < 3:
                nextSound = threading.Thread(target=self._playOne, args=(startedAt,), daemon=True)
                nextSound.start()
                self.threads.append(nextSound)

                time.sleep(2.0 + (random.random() * 8))
            else:
                sys.stdout.write("%.6f: all channels busy\n\r" % time.time())
                time.sleep(0.5)
    
            for t in self.threads:
                if not t.is_alive():
                    t.join()
                    self.threads.remove(t)

        [t.join() for t in self.threads]

    def pause(self):
        self.shouldStop.set()
        if self.controlThread is not None:
            self.controlThread.join()

    def start(self):
        self.resume()

    def resume(self):
        if not self.shouldStop.is_set() and self.controlThread is not None:
            return

        self.shouldStop.clear()
        self.controlThread = threading.Thread(target=self._playContinuously, args=(), daemon=True)
        self.controlThread.start()

        

