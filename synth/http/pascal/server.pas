program SynthServer;
uses
    SysUtils, Sockets;


procedure respond(socket: LongInt);
const
    lookFor: String = '/?note=';
    responseCode: String = 'HTTP/1.1 200 OK';
    contentType: String = 'Content-Type: application/octet-stream';
    crlf: String = #13#10;

var
    buffer: array[0..1023] of Char;
    received, accepted: LongInt;
    request, note, response: String;
    ParamStart, ParamEnd, sent: Integer;

begin
    accepted := fpAccept(socket, nil, nil);
    fillChar(buffer, SizeOf(buffer), 0);
    received := fpRecv(accepted, @buffer, sizeOf(Buffer), 0);
    writeLn('received ', received, ' bytes');
    request := copy(buffer, 1, received);
    ParamStart := pos(lookFor, request) + length(lookFor);
    ParamEnd := pos(' ', copy(request, ParamStart, length(request))) - 1;
    note := copy(request, ParamStart, ParamEnd);
    writeLn('Generating note ', note);
    response := responseCode + crlf + contentType + crlf + 'Content-Length: 3' + crlf + crlf + 'OK' + #10;
    sent := fpSend(accepted, @response + 1, length(response), 0);
    writeLn(sent, ' bytes sent');
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
