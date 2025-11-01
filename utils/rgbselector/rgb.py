#!/usr/bin/python

import RPi.GPIO as gpio
import time


freq = 400
g1 = 16

gpio.setmode(gpio.BCM)
gpio.setup(g1, gpio.OUT)
pg1 = gpio.PWM(g1, freq)
pg1.start(0)
time.sleep(1)
pg1.ChangeDutyCycle(50)
time.sleep(5)


gpio.cleanup()
