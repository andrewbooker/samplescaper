
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Fixed;


procedure Server is
    use GNAT.Sockets;
    use Ada.Streams;

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

    procedure respond_to(note : Integer; stream : Stream_Access) is
        buffer : Stream_Element_Array(1 .. 256) := (others => 0);
        ns : String := Ada.Strings.Fixed.Trim (note'Image, Ada.Strings.Left);
        responseHeader : String :=
              "HTTP/1.1 200 OK" & ASCII.CR & ASCII.LF &
              "Content-Type: text/plain" & ASCII.CR & ASCII.LF &
              "Content-Length: 4" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
    begin
        for i in buffer'Range loop
            if Integer (i) <= responseHeader'Length then
                buffer (i) := Stream_Element (Character'Pos (responseHeader (Integer (i))));
            end if;
        end loop;
        
        buffer (responseHeader'Length + 1) := Stream_Element (Character'Pos (ns (1)));
        buffer (responseHeader'Length + 2) := Stream_Element (Character'Pos (ns (2)));
        buffer (responseHeader'Length + 3) := Stream_Element (Character'Pos (ASCII.CR));
        buffer (responseHeader'Length + 4) := Stream_Element (Character'Pos (ASCII.LF));

        Write (stream.all, buffer);
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
