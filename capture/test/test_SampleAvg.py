from lib.sampleReader import SampleAvg


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
    
