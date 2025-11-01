#!/usr/bin/python

import RPi.GPIO as gpio
import time
import random


class RgbSelector:
    def __init__(self):
        self.freq = 400
        self.port_numbers = [
            (23, 67), (24, 100), (25, 100), (8, 100)
        ]
        self.ports = []
        gpio.setmode(gpio.BCM)
        for p in self.port_numbers:
            gpio.setup(p[0], gpio.OUT)
            self.ports.append((gpio.PWM(p[0], self.freq), p[1]))
            print(f"port {p[0]} PWM up to {p[1]}")
        for p in self.ports:
            p[0].start(0)

    def __del__(self):
        print("shutting down")
        gpio.cleanup()

    def set_to(self, v):
        print(f"setting value {v}")
        for p in self.ports:
            p[0].ChangeDutyCycle(int(v * p[1] / 100.0))

    def set_value(self):
        for p in self.ports:
            v = int((5 + (95 * random.random())) * p[1] / 100.0)
            print(f"setting value {v}")
            p[0].ChangeDutyCycle(v)


selector = RgbSelector()
time.sleep(1)
for i in range(9):
    selector.set_value()
    time.sleep(2)

del selector
