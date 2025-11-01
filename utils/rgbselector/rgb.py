#!/usr/bin/python

import RPi.GPIO as gpio
import time
import random


class RgbSelector:
    def __init__(self):
        self.freq = 400
        self.port_numbers = [16]
        self.ports = []
        gpio.setmode(gpio.BCM)
        for p in self.port_numbers:
            gpio.setup(p, gpio.OUT)
            self.ports.append(gpio.PWM(p, self.freq))
            print(f"port {p} PWM")
        for p in self.ports:
            p.start(0)

    def __del__(self):
        print("shutting down")
        gpio.cleanup()

    def set_to(self, v):
        print(f"setting value {v}")
        for p in self.ports:
            p.ChangeDutyCycle(v)

    def set_value(self):
        for p in self.ports:
            v = 10 + (90 * random.random())
            print(f"setting value {v}")
            p.ChangeDutyCycle(v)


selector = RgbSelector()
time.sleep(1)
for i in range(9):
    selector.set_to((i + 1) * 10)
    time.sleep(2)

del selector
