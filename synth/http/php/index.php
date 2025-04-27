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
        $this->ramp_down = $size * anything_between(0.2, 0.5);
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
    function at(int $i) {
        return 0.0;
    }
}


class Oscillator {
    private float $freq;
    private object $phase;

    function __construct(float $freq, object $phase) {
        $this->freq = $freq;
        $this->phase = $phase;
    }

    function at(int $i) {
        return sin($this->phase->at($i) + (2 * M_PI * $this->freq * $i / SAMPLE_RATE));
    }
}


function oscillator($note) {
    $freq = pow(2.0, ($note - 69) / 12.0) * 440;
    $len = rand(8.0, 20.0);
    $size = $len * SAMPLE_RATE;

    $stdout = fopen("php://stdout", "w");
    fputs($stdout, "generating " . $note . " at " . number_format($freq, 4, '.', '') . "Hz for " . $len . "s\n");

    $phaseLfo = new Oscillator(rand(0.001, 5.2), new ConstVal());
    $osc = new Oscillator($freq, $phaseLfo);
    $envelope = new Envelope($size);
    $wave = array();
    $gain = 3.3;
    for ($i = 0; $i != $size; ++$i) {
        $v = $osc->at($i);
        $sign = $v < 0.0 ? -1.0 : 0.0;
        $wave[$i] = $envelope->at($i) * $sign * min(1.0, $gain * abs($v));
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
    $s = oscillator($note);
    $sound = as_bytes($s);
    $size = count($s) * 4;
    header("Content-Type: application/octet-stream");
    header("Content-Length: " . $size);
    echo $sound;
}
?>
