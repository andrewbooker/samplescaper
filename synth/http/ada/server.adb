
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with GNAT.Sockets;


procedure Server is
    use GNAT.Sockets;
    serverSocket : Socket_Type;
    addr : Sock_Addr_Type;
    port : Port_Type;

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


    Close_Socket (serverSocket);
end Server;
