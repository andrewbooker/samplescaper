
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with GNAT.Sockets;
with Ada.Streams;



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
    begin
        Ada.Text_IO.Put_Line ("request: " & request);
        return 64;
    end read_note_from;

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

        note := read_note_from (read_request (Stream (clientSocket)));
        Ada.Text_IO.Put_Line ("Generating note" & note'Img);
        Close_Socket (clientSocket);
        exit;
    end loop;
    Close_Socket (serverSocket);
    Ada.Text_IO.Put_Line ("Finished");
end Server;
