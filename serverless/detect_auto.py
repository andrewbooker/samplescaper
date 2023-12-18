#!/usr/bin/env python

import time
import RPi.GPIO as GPIO
from enum import Enum
from threading import Thread

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


blinker = BlinkLed()
thr = Thread(target=blinker.blink, args=(), daemon=True)
thr.start()

class State(Enum):
    NORMAL = 1
    AUTO_STARTING = 2
    AUTO_STARTED = 3
    SHUTTING_DOWN = 4


state = State.NORMAL
last_ts = time.time()
should_continue = True
while should_continue:
    button_pressed = not GPIO.input(BUTTON)

    if state == State.SHUTTING_DOWN:
        blinker.set_per_second(10)
        should_continue = False
        print("shutting down")
    if state == State.AUTO_STARTED:
        blinker.set_per_second(1)
        if button_pressed:
            state = State.SHUTTING_DOWN

    if state == State.AUTO_STARTING:
        blinker.set_per_second(4)
        ready = (time.time() - last_ts) >= 10.0
        if ready:
            state = State.AUTO_STARTED
            print("started")

    if state == State.NORMAL and button_pressed:
        state = State.AUTO_STARTING
        last_ts = time.time()
        print("auto-starting", last_ts)

    time.sleep(1)

GPIO.cleanup()
