
class SampleAvg:
    def __init__(self, movingAvgLength, peakLength):
        self.peaks = []
        self.up = True
        self.buffer = []
        self.avgLen = movingAvgLength
        self.peakLen = peakLength
        self.total = 0.0
        self.peakTotal = 0.0

    def add(self, v):
        s = abs(v)
        self.buffer.append(s)
        self.total += s
        if len(self.buffer) > self.avgLen:
            self.total -= self.buffer[0]
            del self.buffer[0]

        if self.up == False and s > self.val():
            self.up = True
        if self.up == True and s < self.val():
            p = self.buffer[-1]
            self.peakTotal += p
            self.peaks.append(p)
            if len(self.peaks) > self.peakLen:
                self.peakTotal -= self.peaks.pop()
            self.up = False

    def clear(self):
        self.total = 0.0
        self.peakTotal = 0.0
        self.buffer = []

    def canAssess(self):
        return len(self.peaks) == self.peakLen

    def val(self):
        if not self.buffer:
            return 0.0
        return self.total / len(self.buffer)

    def peak(self):
        return self.peakTotal / self.peakLen

