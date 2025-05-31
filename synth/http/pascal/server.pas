program SynthServer;

uses
    SysUtils, Sockets, Math;


function frequencyOf(n: integer): single;
begin
    frequencyOf := power(2, (n - 69) / 12.0) * 440;
end;

procedure respond(socket: LongInt);
const
    lookFor: String = '/?note=';
    responseCode: String = 'HTTP/1.1 200 OK';
    contentType: String = 'Content-Type: application/octet-stream';
    crlf: String = #13#10;
    sampleRate: LongInt = 44100;

var
    buffer: array[0..1023] of Char;
    received, accepted: LongInt;
    request, response: String;
    ParamStart, ParamEnd: Integer;
    note: integer;
    sampleTime: real;
    sampleLength: longInt;
    sampleByteLength: longInt;
    sample: single;
    idx: longInt;
    byteLength: longInt;
    responseBytes: array of byte;
    preambleLength: integer;
    freq: single;

begin
    Randomize;
    accepted := fpAccept(socket, nil, nil);
    fillChar(buffer, SizeOf(buffer), 0);
    received := fpRecv(accepted, @buffer, sizeOf(Buffer), 0);
    request := copy(buffer, 1, received);
    ParamStart := pos(lookFor, request) + length(lookFor);
    ParamEnd := pos(' ', copy(request, ParamStart, length(request))) - 1;
    note := strToInt(copy(request, ParamStart, ParamEnd));
    freq := frequencyOf(note);

    sampleTime := 8.0 + (random * 12.0);
    writeLn('Generating note ', note, ' at ', freq:0:2, 'Hz for ', sampleTime:0:2, 's');
    sampleLength := round(sampleTime * sampleRate);
    sampleByteLength := sampleLength * sizeOf(single);

    response := responseCode + crlf + contentType + crlf + 'Content-Length: ' + intToStr(sampleByteLength) + crlf + crlf;
    preambleLength := integer(response[0]);

    byteLength := integer(response[0]) + sampleByteLength;
    setLength(responseBytes, byteLength);
    move(response[1], responseBytes[0], preambleLength);

    for idx := 0 to sampleLength - 1 do
    begin
        sample := sin(idx * 2.0 * pi * freq / sampleRate);
        move(sample, responseBytes[preambleLength + (idx * sizeOf(single))], sizeOf(single));
    end;

    fpSend(accepted, @responseBytes[0], byteLength, 0);
    closeSocket(accepted);
end;


procedure listenOn(port: integer);
var
    socket: LongInt;
    sockAddr: TInetSockAddr;

begin
    socket := fpSocket(AF_INET, SOCK_STREAM, 0);
    if socket < 0 then Halt(1);

    fillChar(sockAddr, sizeOf(sockAddr), 0);
    sockAddr.sin_family := AF_INET;
    sockAddr.sin_port := htons(port);
    sockAddr.sin_addr.s_addr := htonl(INADDR_ANY);

    if fpBind(socket, @sockAddr, sizeOf(sockAddr)) <> 0 then Halt(1);
    if fpListen(socket, 1) <> 0 then Halt(1);

    writeLn('Listening on port ', port);
    while True do respond(socket);
    closeSocket(socket);
end;

var
    port: integer;

begin
    if paramCount > 0 then
        begin
            port := strToInt(paramStr(1));
            listenOn(port)
        end
	else
        writeln('Must supply port number');
end.
