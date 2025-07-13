
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;


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

    function read_request (requestStream : Stream_Access) return String is
        last : Ada.Streams.Stream_Element_Offset;
        buffer : Stream_Element_Array(1 .. 64);
        request : String (1 .. 1024) := (others => Character'Val(0));
        c : Character;
    begin
        Read (requestStream.all, buffer, last);
        Ada.Text_IO.Put_Line ("request length" & last'Img);

        for i in 1 .. last loop
            c := Character'Val (buffer (i));
            if c /= Character'Val (10) then
                request (Positive (i - buffer'First + 1)) := c;
            else
                request (Positive (i - buffer'First + 1)) := Character'Val (0);
                exit;
            end if;
        end loop;

        Ada.Text_IO.Put_Line (request);
        return request;
    end read_request;

    function read_note_from (request : String) return Integer is
        note : String(1 .. 2) := (others => Character'Val(0));
    begin
        note (1) := request (request'First + 11);
        note (2) := request (request'First + 12);
        Ada.Text_IO.Put_Line ("note: " & note);
        return Integer'Value (note);
    end read_note_from;

    function write_audio_to(data : out Stream_Element_Array; note : integer) return integer is
        type Sample is array (1 .. 4) of Stream_Element;
        function To_Bytes is new Ada.Unchecked_Conversion (Source => Float, Target => Sample);
        singleSample : Sample;
        length : integer;
        freq : float;
        value : float;
    begin
        length := 1 * 44100;
        freq := 440.0;

        for i in 1 .. length loop
            value := 1.0 * Sin (2.0 * Pi * freq * float (i) / 44100.0);
            singleSample := To_Bytes (value);
            for j in 1 .. 4 loop
                data (Stream_Element_Offset ((4 * i) + j)) := singleSample (Integer'Val (j));
            end loop;
        end loop;
        return length;
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
        chunk : integer := 512;
    begin
        contentLength := write_audio_to(data, note);
        headerLength := add_to (buffer, baseResponseHeader & "Content-Length:" & contentLength'Img & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

        Write (stream.all, buffer (1 .. Stream_Element_Offset (headerLength)));
        while sent < contentLength loop
            Write (stream.all, data (Stream_Element_Offset (sent + 1) .. Stream_Element_Offset (sent + chunk)));
            sent := sent + chunk;
            chunk := Integer'Min (chunk, contentLength - sent);
        end loop;
    end;

begin
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
        Ada.Text_IO.Put_Line ("Received request");

        declare
            clientStream : Stream_Access := Stream (clientSocket);
        begin
            note := read_note_from (read_request (clientStream));
            Ada.Text_IO.Put_Line ("Generating note" & note'Img);
            respond_to (note, clientStream);
            Close_Socket (clientSocket);
            exit;
        end;
    end loop;
    Close_Socket (serverSocket);
    Ada.Text_IO.Put_Line ("Finished");
end Server;
