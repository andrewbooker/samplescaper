#!/usr/bin/env ruby

require 'socket'

module SynthServer
    SAMPLE_RATE = 44100
end


class RampUpDown
    def initialize(size)
        @rampUp = 44100 * rand(2.0...4.0)
        @rampDown = size * rand(0.2...0.5)
        @startRampDown = size - @rampDown
    end

    def at(i)
        if i < @rampUp
            return 0.5 * (1.0 + Math::cos(Math::PI * (i + @rampUp) / @rampUp))
        end
        if i > @startRampDown
            return 0.5 * (1.0 + Math::cos(Math::PI * (i - @startRampDown) / @rampDown))
        end
        1.0
    end
end

class SineOscillator
    def initialize(freq)
        @freq = freq
    end

    def at(i)
        Math::sin(2.0 * Math::PI * i * @freq / SynthServer::SAMPLE_RATE)
    end
end

class TriangleOscillator
    def initialize(freq, phasor, symmetry)
        @cylesPerSample = freq / SynthServer::SAMPLE_RATE
        @phasor = phasor
        @symmetry = symmetry
    end

    def at(i)
        apex = 0.5 + @symmetry.at(i)
        gradientUp = 2.0 / apex
        gradientDown = -2.0 / (1.0 - apex)
        p = (i * @cylesPerSample) + @phasor.at(i)
        x = p - p.floor()
        up = x <= apex
        up ? (gradientUp * x) - 1.0 : 1.0 + (gradientDown * (x - apex))
    end
end


class Positive
    def initialize(v)
        @v = v
    end

    def at(i)
        0.5 * (1.0 + @v.at(i))
    end
end


class Scaled
    def initialize(scale, v)
        @v = v
        @scale = scale
    end

    def at(i)
        @scale * @v.at(i)
    end
end


class RangeDepth
    def initialize(depth, v)
        @v = Positive.new(v)
        @depth = depth
    end

    def at(i)
        1.0 - (@depth * @v.at(i))
    end
end


class Synth
    def initialize(note)
        @freq = 2.pow((note - 69) / 12.0) * 440;
        @dur = rand(8.0...20.1)
        puts "Generating note #{note} at #{'%.4f' % @freq}Hz for #{'%.4f' % @dur}s"
    end

    def generate
        samples = [].fill(0.0, 0, Integer(SynthServer::SAMPLE_RATE * @dur))
        symmetryLfo = Scaled.new(rand(0.01...0.49), SineOscillator.new(rand(0.001...2.0)))
        phaseLfo = Scaled.new(rand(0.1...0.6), SineOscillator.new(rand(0.001...7.0)))
        synth = TriangleOscillator.new(@freq, phaseLfo, symmetryLfo)
        ramp = RampUpDown.new(samples.length)
        amLfo = RangeDepth.new(rand(0.1...0.7), SineOscillator.new(rand(0.001...4.0)))

        samples.length.times do |i|
            samples[i] = ramp.at(i) * amLfo.at(i) * synth.at(i)
        end

        samples.pack('f*')
    end
end


class HttpServer
    def initialize(port)
        @server = TCPServer.new port
    end

    def accept_connection
        while session = @server.accept
            request = session.gets
            verb, path, _ = request.split(' ')
            note = Integer(path.split("=")[1])

            sound = Synth.new(note).generate
            response = "HTTP/1.1 200\r\nContent-Type: text/html\r\nContent-Length: #{sound.length}\r\n\r\n#{sound}"
            session.print response
        end
    end
end


if ARGV.length === 0
    puts "Must supply port number"
else
    port = Integer(ARGV[0])
    puts "Listening on port #{port}"
    server = HttpServer.new(port)
    loop {
        server.accept_connection
    }
end
