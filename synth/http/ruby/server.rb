#!/usr/bin/env ruby

require 'socket'

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

class Synth
    SAMPLE_RATE = 44100

    def initialize(note)
        @freq = 2.pow((note - 69) / 12.0) * 440;
        @dur = rand(8.0...20.1)
        puts "Generating note #{note} at #{'%.4f' % @freq}Hz for #{'%.4f' % @dur}s"
    end

    def generate
        samples = [].fill(0.0, 0, Integer(SAMPLE_RATE * @dur))
        ramp = RampUpDown.new(samples.length)
        dp = @freq / SAMPLE_RATE
        p = 0.0

        samples.length.times do |i|
            samples[i] = ramp.at(i) * ((2.0 * p) - 1.0)
            p += dp
            if p >= 1.0
                p -= 1.0
            end
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
