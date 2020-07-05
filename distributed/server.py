#!/usr/bin/env python

from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import sys
import datetime
import time
import urllib
import os


audioFile = sys.argv[1]


class AudioFileServer(BaseHTTPRequestHandler):
    
    def do_GET(self):
        with open(audioFile, "rb") as loopFile:
            self.send_response(200)
            self.send_header("Content-type", "audio/x-wav")
            self.wfile.write(loopFile.read())
            self.end_headers()

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
        
        
port = 3064
server = StoppableServer(port)
thread = threading.Thread(target=server.start, args=(), daemon=True)


thread.start()
done = False

import readchar
print("Serving on port %d. Press 'q' to exit" % port)
while not done:
    c = readchar.readchar()
    if c == "q":
        done = True

server.stop()
thread.join()
    

