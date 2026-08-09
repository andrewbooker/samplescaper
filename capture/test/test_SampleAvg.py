from lib.sampleAvg import SampleAvg
from PIL import Image, ImageDraw
import math


WIDTH = 882
SMPL = 44100


class Renderer:
    def __init__(self):
        self.datasets = []

    def add(self, d, col):
        self.datasets.append((d, col))

    def draw(self):
        out = Image.new("RGB", (WIDTH, 200), (0, 0, 0))
        d = ImageDraw.Draw(out)
        for (data, fill) in self.datasets:
            for i in range(0, SMPL):
                d.point((i * 1.0 * WIDTH / SMPL, 100 + (-100 * data[i])), fill=fill)
        out.show()


def test_handles_no_values():
    s = SampleAvg(1, 1)
    assert s.val() == 0.0
    assert s.peak() == 0.0


def test_handles_zero_values():
    s = SampleAvg(2, 2)
    s.add(0)
    s.add(0)
    assert s.val() == 0.0
    assert s.peak() == 0.0


def test_avgs_a_simple_wave_exactly_symmetrical_above_and_below_zero():
    wave = [0,1,0,-1]
    s2 = SampleAvg(2, 10)
    s4 = SampleAvg(4, 10)
    s8 = SampleAvg(8, 10)
    for w in wave:
        s2.add(w)
        s4.add(w)
        s8.add(w)

    assert s2.val() == 0.5
    assert s4.val() == 0.5
    assert s8.val() == 0.5


def test_avgs_a_simple_longer_wave_exactly_symmetrical_above_and_below_zero():
    wave = [0,0.5,1,0.5,0,-0.5,-1,-0.5]
    s2 = SampleAvg(2, 10)
    s4 = SampleAvg(4, 10)
    s8 = SampleAvg(8, 10)
    for w in wave:
        s2.add(w)
        s4.add(w)
        s8.add(w)

    assert s2.val() == 0.25
    assert s4.val() == 0.5
    assert s8.val() == 0.5


def test_handles_a_sinewave():
    data = []
    savg = SampleAvg(SMPL, 10)
    avgVals = []
    peaks = []
    f = 5.0
    for s in range(SMPL):
        d = math.sin(f * 2 * math.pi * s/SMPL)
        data.append(d)
        savg.add(d)
        avgVals.append(savg.val())
        peaks.append(savg.peak())
    
    renderer = Renderer()
    renderer.add(data, "red")
    renderer.add(avgVals, "yellow")
    renderer.add(peaks, "blue")
    renderer.draw()
    
    
    
