#!/usr/bin/env python
import time
import RPi.GPIO as GPIO

GPIO.setmode(GPIO.BCM)
GPIO.setup(21, GPIO.IN, pull_up_down=GPIO.PUD_UP)

should_continue = True
while should_continue:
    state = GPIO.input(21)
    print("On" if state else "Off")
    if not state:
        should_continue = False
    time.sleep(1)
GPIO.cleanup()
