from lib.sampleAvg import SampleAvg


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
