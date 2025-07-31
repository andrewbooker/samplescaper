
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Float_Random;


procedure Server is
    use GNAT.Sockets;
    use Ada.Streams;
    use Ada.Numerics;
    use Ada.Numerics.Elementary_Functions;

    serverSocket : Socket_Type;
    clientSocket : Socket_Type;
    addr : Sock_Addr_Type;
    port : Port_Type;
    note : Integer;
    G : Ada.Numerics.Float_Random.Generator;


    type SineOscillator is tagged record
        freq: float;
    end record;

    function create (f : float) return SineOscillator is
    begin
        return SineOscillator'(freq => f);
    end;

    function valueAt (s: SineOscillator; i: integer) return float is
    begin
        return Sin (2.0 * Pi * s.freq * float (i) / 44100.0);
    end;


    function read_request (requestStream : Stream_Access) return String is
        last : Ada.Streams.Stream_Element_Offset;
        buffer : Stream_Element_Array (1 .. 64);
        request : String (1 .. 1024) := (others => Character'Val(0));
        c : Character;
    begin
        Read (requestStream.all, buffer, last);

        for i in 1 .. last loop
            c := Character'Val (buffer (i));
            if c /= Character'Val (10) then
                request (Positive (i - buffer'First + 1)) := c;
            else
                request (Positive (i - buffer'First + 1)) := Character'Val (0);
                exit;
            end if;
        end loop;
        return request;
    end;

    function read_note_from(request : String) return Integer is
        note : String(1 .. 2) := (others => Character'Val(0));
    begin
        note (1) := request (request'First + 11);
        note (2) := request (request'First + 12);
        return Integer'Value (note);
    end;


    function random_value_between(l: float; u: float) return float is
    begin
        return l + ((u - l) * Ada.Numerics.Float_Random.Random (G));
    end;

    function ramp_at(i: float; length: float; ramp_up: float; ramp_down: float) return float is
        start_ramp_down : float := length - ramp_down;
    begin
        if i < ramp_up then
            return 0.5 * (1.0 + Cos (Pi * (i + ramp_up) / ramp_up));
        elsif i > start_ramp_down then
            return 0.5 * (1.0 + Cos (Pi * (i - start_ramp_down) / ramp_down));
        end if;
        return 1.0;
    end;

    procedure report(note: integer; freq: float; durSecs: float) is
        package F_IO is new Ada.Text_IO.Float_IO (float);
    begin
        Ada.Text_IO.Put ("generating" & note'Img & " at ");
        F_IO.Put(Item => freq, Fore => 1, Aft => 4, Exp => 0);
        Ada.Text_IO.Put ("Hz for ");
        F_IO.Put(Item => durSecs, Fore => 1, Aft => 4, Exp => 0);
        Ada.Text_IO.Put_Line ("s");
    end;

    function write_audio_to(data : out Stream_Element_Array; note : integer) return integer is
        type Sample is array (1 .. 4) of Stream_Element;
        function To_Bytes is new Ada.Unchecked_Conversion (Source => Float, Target => Sample);
        singleSample : Sample;
        durSecs : float := random_value_between (8.0, 16.0);
        length : float := 44100.0 * durSecs;
        intLen : integer := integer (length + 0.5);
        ramp_up : float := length * random_value_between (0.1, 0.3);
        ramp_down : float := length * random_value_between (0.3, 0.5);
        freq : float := (2.0 ** (float (note - 69) / 12.0)) * 440.0;
        value : float;
        oscillator : SineOscillator := create (freq);
    begin
        report (note, freq, durSecs);

        for i in 1 .. intLen loop
            value := ramp_at (float (i), length, ramp_up, ramp_down) * valueAt (oscillator, i);
            singleSample := To_Bytes (value);
            for j in 1 .. 4 loop
                data (Stream_Element_Offset ((4 * i) + j)) := singleSample (Integer'Val (j));
            end loop;
        end loop;
        return intLen * 4;
    end;

    function add_to(buffer : out Stream_Element_Array; header : String) return integer is
    begin
        for i in buffer'Range loop
            if Integer (i) <= header'Length then
                buffer (i) := Stream_Element (Character'Pos (header (Integer (i))));
            end if;
        end loop;
        return header'Length;
    end;

    procedure respond_to(note : Integer; stream : Stream_Access) is
        buffer : Stream_Element_Array(1 .. 256) := (others => 0);
        data : Stream_Element_Array(1 .. 44100 * 20 * 4) := (others => 0);
        baseResponseHeader : constant String := "HTTP/1.1 200 OK" & ASCII.CR & ASCII.LF & "Content-Type: application/octet-stream" & ASCII.CR & ASCII.LF;
        headerLength : integer;
        contentLength : integer;
        sent : integer := 0;
        chunk : integer := 1024;
    begin
        contentLength := write_audio_to(data, note);
        headerLength := add_to (buffer, baseResponseHeader & "Content-Length:" & contentLength'Img & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
        Write (stream.all, buffer (1 .. Stream_Element_Offset (headerLength)));
        Write (stream.all, data (1 .. Stream_Element_Offset (contentLength)));
    end;

begin
    Ada.Numerics.Float_Random.Reset (G);
    port := Port_Type (Integer'Value (Ada.Command_Line.Argument (1)));
    GNAT.Sockets.Initialize;
    Create_Socket (serverSocket, Family_Inet, Socket_Stream);
    Set_Socket_Option (serverSocket, Socket_Level, (Reuse_Address, True));
    addr.Addr := Any_Inet_Addr;
    addr.Port := port;
    Bind_Socket (serverSocket, addr);
    Listen_Socket (serverSocket);

    Ada.Text_IO.Put_Line ("Listening on port" & port'Img);

    loop
        Accept_Socket (serverSocket, clientSocket, addr);

        declare
            clientStream : Stream_Access := Stream (clientSocket);
        begin
            note := read_note_from (read_request (clientStream));
            respond_to (note, clientStream);
        end;
    end loop;
    Close_Socket (clientSocket);
    Close_Socket (serverSocket);
    Ada.Text_IO.Put_Line ("Finished");
end Server;
