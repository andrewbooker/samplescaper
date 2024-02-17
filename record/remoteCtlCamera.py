#!/usr/bin/env python
import requests
import xml.etree.ElementTree as xt
import re
import time

class FileStatusChecker:
    def __init__(self, folder_url):
        self.folder_url = folder_url

    def latest(self):
        listing_response = requests.get(self.folder_url)
        if listing_response.status_code != 200:
            print("cannot list files in remote device", listing_response.status_code)
            return None

        text_lines = listing_response.text.split("\n")
        files = []
        for t in text_lines:
            if "href" in t and "Remove" not in t:
                fn = re.search(r"\d{4}_\d{4}_\d{6}", t).group()
                size = float(re.search(r"\d+\.\d+\s+MB", t).group().split(" MB")[0])
                files.append((fn, size))

        files.sort(key=lambda i: i[0], reverse=True)
        return files[0]


camera_url = "http://192.168.1.254/"

file_status_checker = FileStatusChecker(f"{camera_url}CARDV/MOVIE")
last_file = None
last_size = None
while True:
    latest = file_status_checker.latest()
    if last_file is None:
        last_file, last_size = latest
        print("found", latest)
    else:
        if last_file != latest[0] or last_size < latest[1]:
            print("Video already running")
        elif last_file == latest[0] and last_size == latest[1]:
            print("Video not yet running. Starting...")
            r = requests.get(camera_url, params=[('custom', '1'), ('cmd', 2001), ('str', 1)])
            if r.status_code == 200:
                print("Video started")
            else:
                print("Start failed", r.status_code, r.text)
        exit()
    time.sleep(1.08482)
print("done")

