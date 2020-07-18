#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler, SimpleHTTPRequestHandler
import threading
import sys
import time
import urllib
import os
import sys
import random


def nextAudioFile():
    poolDir = sys.argv[1]
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    return os.path.join(os.path.join(poolDir, files[random.randint(0, len(files) - 1)]))


class AudioFileServer(BaseHTTPRequestHandler):
    def do_GET(self):
        audioFile = nextAudioFile()
        size = os.stat(audioFile).st_size
        with open(audioFile, "rb") as loopFile:
            self.send_response(200)
            self.send_header("Content-Type", "audio/x-wav")
            self.send_header("Content-Length", size)
            self.end_headers()
            self.wfile.write(loopFile.read())
        sys.stdout.write("\r")
        return None

class StaticHtmlServer(SimpleHTTPRequestHandler):
    def do_GET(self):
        self.path = "./client.html"
        return SimpleHTTPRequestHandler.do_GET(self)

class StoppableServer:
    def __init__(self, port, servlet):
        self.port = port
        self.servlet = servlet

    def start(self):
        self.httpd = HTTPServer(("0.0.0.0", self.port), self.servlet)
        self.httpd.serve_forever()
        
    def stop(self):
        print("stopping...")
        self.httpd.shutdown()
        self.httpd.server_close()
        print("stopped")

random.seed()

servers = [StoppableServer(3064, AudioFileServer), StoppableServer(3065, StaticHtmlServer)]
threads = []

for s in servers:
    threads.append(threading.Thread(target=s.start, args=(), daemon=True))

[t.start() for t in threads]
done = False

import readchar
print("Started. Press 'q' to exit")
while not done:
    c = readchar.readchar()
    if c == "q":
        done = True

[s.stop() for s in servers]
[t.join() for t in threads]
    

