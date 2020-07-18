#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import sys
import time
import urllib
import os
import random


def nextAudioFile():
    audioFileDir = sys.argv[1]
    notes = [50, 52, 53, 55, 57, 58, 60, 62, 64, 65]
    note = notes[random.randint(0, len(notes) - 1)]
    noteDir = os.path.join(audioFileDir, str(note))
    files = [f for f in filter(lambda f: "wav" in f, os.listdir(noteDir))]
    return os.path.join(os.path.join(noteDir, files[random.randint(0, len(files) - 1)]))


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

class StoppableServer:
    def __init__(self, port):
        self.port = port

    def start(self):
        self.httpd = HTTPServer(("0.0.0.0", self.port), AudioFileServer)
        self.httpd.serve_forever()
        
    def stop(self):
        print("stopping...")
        self.httpd.shutdown()
        self.httpd.server_close()
        print("stopped")

random.seed()
port = 3064
server = StoppableServer(port)
thread = threading.Thread(target=server.start, args=(), daemon=True)

thread.start()
done = False


import http.server
import socketserver

class ClientHtmlServer(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        self.path = "./client.html"
        return http.server.SimpleHTTPRequestHandler.do_GET(self)

my_server = socketserver.TCPServer(("0.0.0.0", 3065), ClientHtmlServer)
my_server.serve_forever()







import readchar
print("Serving on port %d. Press 'q' to exit" % port)
while not done:
    c = readchar.readchar()
    if c == "q":
        done = True

server.stop()
thread.join()
    

