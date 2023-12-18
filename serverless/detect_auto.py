#!/usr/bin/env python

import time
import RPi.GPIO as GPIO
from enum import Enum
from threading import Thread
import requests

LED = 16
BUTTON = 20

GPIO.setmode(GPIO.BCM)
GPIO.setup(BUTTON, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(LED, GPIO.OUT)

class BlinkLed:
    def __init__(self):
        self.per_second = 0.0

    def blink(self):
        while True:
            if self.per_second == 0.0:
                GPIO.output(LED, 0)
                time.sleep(1.0)
            else:
                GPIO.output(LED, 1)
                time.sleep(0.05)
                GPIO.output(LED, 0)
                time.sleep(max(0.1, (1.0 / self.per_second) - 0.05))

    def set_per_second(self, ps):
        self.per_second = ps


class ControllerClient:
    def __init__(self):
        self.url = "http://0.0.0.0"

    def get_status(self):
        try:
            response = requests.get(f"{self.url}:9966")
            if response.status_code == 200:
                return response.json()["state"]
        except requests.exceptions.ConnectionError as e:
            print(e)
        return "not_available"

    def play_sound(self):
        try:
            response = requests.post(f"{self.url}:9966/play")
        except requests.exceptions.ConnectionError as e:
            print(e)

    def start_rotation(self):
        try:
            response = requests.post(f"{self.url}:9977/random")
            if response.status_code == 200:
                return True
        except requests.exceptions.ConnectionError as e:
            print(e)
        return False

    def shutdown(self):
        requests.post(f"{self.url}:9966/shutdown")


blinker = BlinkLed()
thr = Thread(target=blinker.blink, args=(), daemon=True)
thr.start()

controller_client = ControllerClient()

class State(Enum):
    NORMAL = 1
    MOTOR_STARTING = 2
    MOTOR_STARTED = 3
    SOUND_STARTED = 4
    SHUTTING_DOWN = 5


state = State.NORMAL
should_continue = True
print("listening for auto-start command")
while should_continue:
    button_pressed = not GPIO.input(BUTTON)

    if state == State.SHUTTING_DOWN:
        blinker.set_per_second(10)
        should_continue = False
        print("shutting down")

    if state == State.SOUND_STARTED:
        blinker.set_per_second(1)
        if button_pressed:
            state = State.SHUTTING_DOWN

    if state == State.MOTOR_STARTED:
        controller_status = controller_client.get_status()
        print("controller status:", controller_status)
        if controller_status == "playing":
            state = State.SOUND_STARTED
            print("already playing")
        if controller_status == "ready":
            controller_client.play_sound()
            state = State.SOUND_STARTED
            print("playing")

    if state == State.MOTOR_STARTING:
        blinker.set_per_second(4)
        controller_client.start_rotation()
        state = State.MOTOR_STARTED
        print("rotation started")

    if state == State.NORMAL and button_pressed:
        state = State.MOTOR_STARTING
        print("starting")

    time.sleep(1)

GPIO.cleanup()
controller_client.shutdown()

