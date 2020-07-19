#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler, SimpleHTTPRequestHandler
import threading
import sys
import time
import urllib
import os
import sys
import random
import json


def nextAudioFile():
    poolDir = sys.argv[1]
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(poolDir))]
    return os.path.join(poolDir, files[random.randint(0, len(files) - 1)])


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


class Discoverer(BaseHTTPRequestHandler):
    def do_HEAD(self):
        fn = "./servers.json"
        known = []
        if os.path.exists(fn) and os.stat(fn).st_size > 2:
            with open(fn, "r") as sl:
                known += json.load(sl)

        found = self.client_address[0]
        print("discovered by", found)
        updated = [found]
        [updated.append(s) for s in filter(lambda h: h not in ["127.1.1.0", "localhost", found], known)]
        if updated != known:
            with open(fn, "w") as sl:
                json.dump(updated, sl)
        sys.stdout.write("\r")
        self.send_response(200)
        self.end_headers()
        return None

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

servers = []
servers.append(StoppableServer(3064, AudioFileServer))
servers.append(StoppableServer(3065, StaticHtmlServer))
servers.append(StoppableServer(3066, Discoverer))
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
    

