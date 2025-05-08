#!/usr/bin/perl

use strict;
use warnings;

{
    package System;
    use constant SAMPLE_RATE => 44100;
}

{
    package ValueAt;
    sub at {}
}

{
    package ConstVal;
    use base qw(ValueAt);

    sub of {
        my ($class, $v) = @_;
        bless { v => $v }, $class
    }

    sub at {
        my ($self, $i) = @_;
        $self->{v}
    }
}

{
    package Oscillator;
    use base qw(ValueAt);

    sub _possible {
        my $val = shift;
        if (defined $val) {
            return $val;
        }
        ConstVal->of(0.0)
    }

    sub new {
        my ($class, $freq, $phase_osc) = @_;
        my $self = {
            iterations_per_cycle => (System::SAMPLE_RATE * 1.0) / $freq,
            phase => _possible($phase_osc)
        };
        bless $self, $class
    }

    sub _pos_at {
        my ($self, $i) = @_;
        my $pos = $i / $self->{iterations_per_cycle};
        $pos - int($pos)
    }
}

{
    package SineOscillator;
    use Math::Trig;
    use base qw(Oscillator);

    sub at {
        my ($self, $i) = @_;
        sin($self->{phase}->at($i) + (2 * pi * $self->_pos_at($i)))
    }
}

{
    package TriangleOscillator;
    use base qw(SineOscillator);

    sub at {
        my ($self, $i) = @_;
        my $sv = $self->SUPER::at($i);
        my $p = 4.0 * $self->SUPER::_pos_at($i);

        if ($sv < 0.0) {
            return 3.0 - $p;
        }
        $p - 1.0
    }
}

{
    package CircularOscillator;
    use base qw(Oscillator);

    sub _from {
        my $v = shift;
        sqrt(1.0 - ($v ** 2))
    }

    sub at {
        my ($self, $i) = @_;
        my $pos = $self->SUPER::_pos_at($i);
        my $sw = 0.5 + (0.4 * $self->{phase}->at($i));
        if ($pos < $sw) {
            my $sc = 4.0 * 0.5 / $sw;
            return CircularOscillator::_from(($sc * $pos) - 1.0);
        }
        my $sc = 4.0 * (0.5 / (1.0 - $sw));
        -CircularOscillator::_from(($sc * ($pos - $sw)) - 1.0);
    }
}

{
    package Scaled;
    use base qw(ValueAt);

    sub new {
        my ($class, $depth, $base_osc) = @_;
        my $self = {
            depth => $depth,
            base_oscillator => $base_osc
        };
        bless $self, $class
    }

    sub at {
        my ($self, $i) = @_;
        (1.0 - $self->{depth}) + (0.5 * $self->{depth} * (1.0 + $self->{base_oscillator}->at($i)));
    }
}

{
    package RampUpDown;
    use Math::Trig;
    use base qw(ValueAt);

    sub new {
        my ($class, $sample_len) = @_;

        my $ramp_up = int((2.0 + rand(2.0)) * System::SAMPLE_RATE);
        my $ramp_down = int($sample_len * (0.2 + rand(0.3)));
        my $self = {
            ramp_up => $ramp_up,
            ramp_down => $ramp_down,
            start_ramp_down => $sample_len - $ramp_down
        };
        bless $self, $class
    }

    sub at {
        my ($self, $i) = @_;
        if ($i < $self->{ramp_up}) {
            return 0.5 * (1.0 + cos(pi * ($i + $self->{ramp_up}) / $self->{ramp_up}));
        }
        if ($i > $self->{start_ramp_down}) {
            return 0.5 * (1.0 + cos(pi * ($i - $self->{start_ramp_down}) / $self->{ramp_down}));
        }
        1.0
    }
}

{
    package Synth;

    sub _frequency_of {
        my $note = shift;
        (2 ** (($note - 69) / 12.0)) * 440;
    }

    sub generate {
        my $note = shift;
        my $f = _frequency_of($note);
        my $lfo_phase = SineOscillator->new(0.001 + rand(3.0));
        my $synth = CircularOscillator->new($f, $lfo_phase);
        my $s = 8.0 + rand(12.0);
        print STDERR ("generating $note at ", sprintf("%.4fHz", $f), " for ", sprintf("%.4fs\n", $s));
        my $sample_len = int($s * System::SAMPLE_RATE);
        my $ramp = RampUpDown->new($sample_len);
        my $lfo_am = Scaled->new(rand(0.9), SineOscillator->new(0.001 + rand(5.0)));
        my @buffer = (0.0) x $sample_len;
        for my $i (0..$sample_len) {
            $buffer[$i] = $ramp->at($i) * $lfo_am->at($i) * $synth->at($i);
        }
        @buffer
    }
}

{
    package Server;
    use HTTP::Server::Simple::CGI;
    use base qw(HTTP::Server::Simple::CGI);

    sub handle_request {
        my ($self, $cgi) = @_;
        my $note = $cgi->param("note");

        my @buffer = Synth::generate($note);
        my $l = @buffer;
        my $cl = $l * 4;
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Length: $cl\r\nContent-Type: application/octet-stream\r\n\r\n";
        print pack('f' x $cl, @buffer);
    }
}

my $port = $ARGV[0];
my $server = Server->new($port);
$server->run;
