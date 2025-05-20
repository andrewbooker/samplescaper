#!/usr/bin/env ruby

require 'socket'
class HttpServer
    def initialize(port)
        @server = TCPServer.new port
    end

    def response
        r = [0.0, 0.0, 0.0, 0.0].pack('f*')
        "HTTP/1.1 200\r\nContent-Type: text/html\r\nContent-Length: #{r.length}\r\n\r\n#{r}"
    end

    def accept_connection
        while session = @server.accept
            request = session.gets
            verb, path, _ = request.split(' ')
            note = Integer(path.split("=")[1])
            puts "#{verb} request received for note #{note}"
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
