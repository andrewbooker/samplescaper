<?php

const SAMPLE_RATE = 44100;

function anything_between(float $l, float $u) {
    return $l + (($u - $l) * rand() * 1.0 / getrandmax());
}


class Envelope {
    private int $ramp_up;
    private int $ramp_down;
    private int $start_ramp_down;

    function __construct(int $size) {
        $this->ramp_up = SAMPLE_RATE * anything_between(2.0, 4.0);
        $this->ramp_down = $size * anything_between(0.3, 0.5);
        $this->start_ramp_down = $size - $this->ramp_down;
    }

    function at(int $i) {
        if ($i < $this->ramp_up) {
            return 0.5 * (1.0 + cos(M_PI * ($i + $this->ramp_up) / $this->ramp_up));
        }
        if ($i > $this->start_ramp_down) {
            return 0.5 * (1.0 + cos(M_PI * ($i - $this->start_ramp_down) / $this->ramp_down));
        }
        return 1.0;
    }
}

class ConstVal {
    private float $val;

    private function __construct($v) {
        $this->val = $v;
    }
    static function of($v) {
        return new ConstVal($v);
    }
    function at(int $ignore) {
        return $this->val;
    }
}


class Positive {
    private object $modulator;

    private function __construct(object $modulator) {
        $this->modulator = $modulator;
    }

    static function of($mod) {
        return new Positive($mod);
    }

    function at(int $i) {
        return 0.5 * (1.0 + $this->modulator->at($i));
    }
}


class Depth {
    private float $depth;
    private object $modulator;

    private function __construct($d, $m) {
        $this->depth = $d;
        $this->modulator = $m;
    }

    static function of($d, $mod) {
        return new Depth($d, $mod);
    }

    function at(int $i) {
        return $this->depth + ((1.0 - $this->depth) * $this->modulator->at($i));
    }
}


class Scaled {
    private float $coeff;
    private object $modulator;

    private function __construct($c, $m) {
        $this->coeff = $c;
        $this->modulator = $m;
    }

    static function by($c, $mod) {
        return new Scaled($c, $mod);
    }

    function at(int $i) {
        return $this->coeff * $this->modulator->at($i);
    }
}


class Oscillator {
    private float $freq;
    private object $phase;
    private object $amplitude;

    function __construct(float $freq, object $phase, object $amp) {
        $this->freq = $freq;
        $this->phase = $phase;
        $this->amplitude = $amp;
    }

    function at(int $i) {
        return $this->amplitude->at($i) * sin($this->phase->at($i) + (2 * M_PI * $this->freq * $i / SAMPLE_RATE));
    }
}


function generate($note) {
    $freq = pow(2.0, ($note - 69) / 12.0) * 440;
    $len = anything_between(8.0, 20.0);
    $size = floor($len * SAMPLE_RATE);

    $stdout = fopen("php://stdout", "w");
    fputs($stdout, "generating " . $note . " at " . number_format($freq, 4, '.', '') . "Hz for " . $len . "s\n");

    $zero = ConstVal::of(0.0);
    $amplitudeLfo = new Oscillator(anything_between(0.001, 7.0), $zero, ConstVal::of(1.0));
    $phaseLfo = new Oscillator(anything_between(0.001, 5.2), $zero, ConstVal::of(anything_between(0.1, 2.0)));
    $osc = new Oscillator($freq, $phaseLfo, Depth::of(anything_between(0.0, 1.0), Positive::of($amplitudeLfo)));
    $envelope = Scaled::by(0.9, new Envelope($size));
    $wave = array();
    $gainLfo = Scaled::by(anything_between(1.0, 20.0), Depth::of(anything_between(0.0, 1.0), Positive::of(new Oscillator(anything_between(0.0001, 2.3), $zero, ConstVal::of(1.0)))));
    for ($i = 0; $i != $size; ++$i) {
        $v = $osc->at($i);
        $sign = $v < 0.0 ? -1.0 : 0.0;
        $wave[$i] = $envelope->at($i) * $sign * min(1.0, $gainLfo->at($i) * abs($v));
    }
    return $wave;
}


function as_bytes(array $numbers) {
    $bytes = "";
    foreach ($numbers as $f) {
        $bytes .= pack("f", $f);
    }
    return $bytes;
}


$p = parse_url($_SERVER["REQUEST_URI"], PHP_URL_QUERY);
$params = explode("=", $p);
if ($params[0] == "note") {
    $note = $params[1];
    $s = generate($note);
    $sound = as_bytes($s);
    $size = count($s) * 4;
    header("Content-Type: application/octet-stream");
    header("Content-Length: " . $size);
    echo $sound;
}
?>
