
class SampleAvg:
    def __init__(self, movingAvgLength):
        self.buffer = []
        self.avgLen = movingAvgLength
        self.total = 0.0

    def add(self, v):
        s = abs(v)
        self.buffer.append(s)
        self.total += s
        if len(self.buffer) > self.avgLen:
            self.total -= self.buffer[0]
            del self.buffer[0]

    def val(self):
        if not self.buffer:
            return 0.0
        return self.total / len(self.buffer)

