program SynthServer;

uses
    SysUtils, Sockets, Math;


function frequencyOf(n: integer): single;
begin
    frequencyOf := power(2, (n - 69) / 12.0) * 440;
end;

function anythingBetween(s, e: single): single;
begin
    anythingBetween := s + ((e - s) * random);
end;


// SineOscillator

type
    SineOscillator = object
    private
        freq: single;
    procedure init(f: single);
    function at(i: longInt): single;
end;

procedure SineOscillator.init(f: single);
begin
    freq := f;
end;

function SineOscillator.at(i: longInt): single;
begin
    at := sin(2.0 * pi * freq * i / 44100);
end;



function positive(v: single): single;
begin
    positive := 0.5 * (1.0 + v);
end;


// MixedCircleOscillator

type
    MixedCircleOscillator = object
    private
        freq, cyclesPerIteration: single;
        symmetry: SineOscillator;

    procedure init(f: single; s: SineOscillator);
    function at(i: longInt): single;
end;

procedure MixedCircleOscillator.init(f: single; s: SineOscillator);
begin
    freq := f;
    cyclesPerIteration := f / 44100;
    symmetry := s;
end;

function MixedCircleOscillator.at(i: longInt): single;
var
    p, pos, sw, sc: single;
begin
    p := i * cyclesPerIteration;
    pos := p - trunc(p);
    sw := positive(0.9 * symmetry.at(i));

    if pos < sw then
        begin
            sc := 4.0 * 0.5 / sw;
            at := sqrt(1.0 - power((sc * pos) - 1.0, 2))
        end
    else
        begin
            sc := 4.0 * (0.5 / (1.0 - sw));
            at := -0.5 - (0.5 * sqrt(1.0 - power((sc * (pos - sw)) - 1.0, 2)))
        end
end;


// RampUpDown

type
    RampUpDown = object
    private
        rampUp, rampDown, startRampDown: longInt;
    public
    procedure init(l: longInt);
    function at(i: longInt): single;
    end;

procedure RampUpDown.init(l: longInt);
begin
    rampUp := round(anythingBetween(2.0, 5.0) * 44100);
    rampDown := round(anythingBetween(0.2, 0.5) * l);
    startRampDown := l - rampDown;
end;

function RampUpDown.at(i: longInt): single;
begin
    if i < rampUp then
        at := 0.5 * (1.0 + cos(pi * (i + rampUp) / rampUp))
    else if i > startRampDown then
        at := 0.5 * (1.0 + cos(pi * (i - startRampDown) / rampDown))
    else
        at := 1.0;
end;


procedure respond(accepted: longInt; request: string);
const
    lookFor: String = '/?note=';
    responseCode: String = 'HTTP/1.1 200 OK';
    contentType: String = 'Content-Type: application/octet-stream';
    crlf: String = #13#10;
    sampleRate: LongInt = 44100;

var
    response: String;
    ParamStart, ParamEnd: Integer;
    note: integer;
    sampleTime: real;
    sampleLength, sampleByteLength: longInt;
    sample, freq: single;
    idx, byteLength: longInt;
    responseBytes: array of byte;
    preambleLength: integer;
    ramp: RampUpDown;
    osc: MixedCircleOscillator;
    symmetry: SineOscillator;

begin
    Randomize;
    ParamStart := pos(lookFor, request) + length(lookFor);
    ParamEnd := pos(' ', copy(request, ParamStart, length(request))) - 1;
    note := strToInt(copy(request, ParamStart, ParamEnd));
    freq := frequencyOf(note);

    sampleTime := anythingBetween(8.0, 20.0);
    writeLn('Generating note ', note, ' at ', freq:0:2, 'Hz for ', sampleTime:0:2, 's');
    sampleLength := round(sampleTime * sampleRate);
    sampleByteLength := sampleLength * sizeOf(single);
    response := responseCode + crlf + contentType + crlf + 'Content-Length: ' + intToStr(sampleByteLength) + crlf + crlf;
    preambleLength := integer(response[0]);

    byteLength := integer(response[0]) + sampleByteLength;
    setLength(responseBytes, byteLength);
    move(response[1], responseBytes[0], preambleLength);
    ramp.init(sampleLength);
    symmetry.init(anythingBetween(0.01, 5.0));
    osc.init(freq, symmetry);
    for idx := 0 to sampleLength - 1 do
    begin
        sample := ramp.at(idx) * osc.at(idx);
        move(sample, responseBytes[preambleLength + (idx * sizeOf(single))], sizeOf(single));
    end;

    fpSend(accepted, @responseBytes[0], byteLength, 0);
    closeSocket(accepted);
end;


function serve(socket: LongInt): boolean;
var
    accepted: longInt;
    buffer: array[0..1023] of Char;
    received: LongInt;
    request: String;

begin
    accepted := fpAccept(socket, nil, nil);
    fillChar(buffer, SizeOf(buffer), 0);
    received := fpRecv(accepted, @buffer, sizeOf(Buffer), 0);
    request := copy(buffer, 1, received);
    if pos('die', request) > 0 then
        serve := true
    else
        begin
            respond(accepted, request);
            serve := false;
        end;
end;


procedure listenOn(port: integer);
var
    socket: LongInt;
    sockAddr: TInetSockAddr;
    done: boolean;

begin
    socket := fpSocket(AF_INET, SOCK_STREAM, 0);
    if socket < 0 then
    begin
        writeLn('Could not open server socket');
        Halt(1);
    end;

    fillChar(sockAddr, sizeOf(sockAddr), 0);
    sockAddr.sin_family := AF_INET;
    sockAddr.sin_port := htons(port);
    sockAddr.sin_addr.s_addr := htonl(INADDR_ANY);

    if fpBind(socket, @sockAddr, sizeOf(sockAddr)) <> 0 then
    begin
        writeLn('Could bind server socket');
        Halt(1);
    end;

    if fpListen(socket, 1) <> 0 then Halt(1);

    writeLn('Listening on port ', port);
    repeat
        done := serve(socket);
    until done;
    writeLn('Closing server socket');
    closeSocket(socket);
end;

var
    port: integer;

begin
    if paramCount > 0 then
        begin
            port := strToInt(paramStr(1));
            listenOn(port);
            writeLn('Stopped');
        end
	else
        writeln('Must supply port number');
end.
