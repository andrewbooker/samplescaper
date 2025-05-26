program SynthServer;
uses
    SysUtils;

var
    port: integer;

begin
    if paramCount > 0 then
    begin
        port := strToInt(paramStr(1));
	    writeLn('Listening on port ', port);
    end
	else
        writeln('Must supply port number');
end.
