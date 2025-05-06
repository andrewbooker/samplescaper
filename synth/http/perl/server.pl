#!/usr/bin/perl

use strict;
use warnings;


{
    package Synth;
    use constant SAMPLE_RATE => 44100;
    use Math::Trig;

    sub _frequency_of {
        my $note = shift;
        (2 ** (($note - 69) / 12.0)) * 440;
    }

    sub generate {
        my $note = shift;
        my $f = _frequency_of($note);
        my $s = 8.0 + rand(12.0);
        print STDERR ("generating $note at ", sprintf("%.4fHz", $f), " for ", sprintf("%.4fs\n", $s));
        my $sample_len = int($s * SAMPLE_RATE);
        my @buffer = (0.0) x $sample_len;
        for my $i (0..$sample_len) {
            $buffer[$i] = sin(2 * pi * $f * $i / SAMPLE_RATE);
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
