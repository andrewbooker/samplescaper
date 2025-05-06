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
    package Oscillator;
    use Math::Trig;
    use base qw(ValueAt);

    sub new {
        my ($class, $freq) = @_;
        my $self = {
            freq => $freq
        };
        return bless $self, $class;
    }

    sub at {
        my ($self, $i) = @_;
        sin(2 * pi * $self->{freq} * $i / System::SAMPLE_RATE)
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
        return bless $self, $class;
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
        my $synth = Oscillator->new($f);
        my $s = 8.0 + rand(12.0);
        print STDERR ("generating $note at ", sprintf("%.4fHz", $f), " for ", sprintf("%.4fs\n", $s));
        my $sample_len = int($s * System::SAMPLE_RATE);
        my $ramp = RampUpDown->new($sample_len);
        my @buffer = (0.0) x $sample_len;
        for my $i (0..$sample_len) {
            $buffer[$i] = $ramp->at($i) * $synth->at($i);
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
        print STDERR "returning $cl bytes\n";
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Length: $cl\r\nContent-Type: application/octet-stream\r\n\r\n";
        print pack('f' x $cl, @buffer);
    }
}

my $port = $ARGV[0];
my $server = Server->new($port);
$server->run;
